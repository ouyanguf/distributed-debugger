import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class LogHandler {

	public static void main(String args[]) throws Exception {
		String inputFolder = "E://EclipseWorkspace//DOS-Project4-Log";
		String outputFolder = "E://EclipseWorkspace//DOS-Project4-Log";
		if (args.length == 2) {
			inputFolder = args[0];
			outputFolder = args[1];
		}
		process(inputFolder, outputFolder);

	}

	public static void process(String inputF, String outputF) throws Exception {
		File inputFolder = new File(inputF);
		File outputFolder = new File(outputF);
		File rawAllLog = new File(outputFolder.getPath() + "//all.log");
		File inOrderLog = new File(outputFolder.getPath()
				+ "//all-in-order.log");
		deleteContent(rawAllLog);
		deleteContent(inOrderLog);
		int numFiles = FileMerger.fileCombine(rawAllLog, inputFolder);
		System.out.println("Raw Log File Created!");
		logsHand(numFiles, rawAllLog, outputFolder, inOrderLog, inputFolder,
				false);
	}

	public static class Msg implements Comparable<Msg> {
		int Priority;
		String msg;

		public Msg(int P, String M) {
			this.Priority = P;
			this.msg = M;
		}

		public int compareTo(Msg other) {
			if (this.Priority < other.Priority) {
				return -1;
			} else if (this.Priority == other.Priority) {
				return 0;
			} else {
				return 1;
			}
		}

	}

	public static void logsHand(int ActorNum, File alllog, File outputFolder,
			File inOrderLog, File inputFolder, boolean makeImg)
			throws Exception {
		File error = new File(outputFolder.getPath() + "//all-error.log");
		FileReader fileIn = new FileReader(alllog);
		@SuppressWarnings("resource")
		Scanner scan = new Scanner(fileIn);
		ArrayList<Msg> store = new ArrayList<Msg>();
		while (scan.hasNext()) {
			int p = scan.nextInt();
			String msg = scan.nextLine();
			Msg temp = new Msg(p, msg);
			store.add(temp);
		}
		Collections.sort(store);

		// Make one way message log file or report error
		// System.out.println(store.size());
		int oldSize = store.size();
		ArrayList<Msg> oneWay = new ArrayList<Msg>();
		while (store.size() > 0) {
			boolean found = false;
			String[] mySegs = store.get(0).msg.split(" "); // 1-actorx 2-=>
															// 3-actory
			// System.out.println(segs[2]);
			for (int i = 1; i < store.size(); i++) {
				String[] theSegs = store.get(i).msg.split(" ");
				if (theSegs[2].equals("<=") && theSegs[1].equals(mySegs[3])
						&& theSegs[3].equals(mySegs[1])
						&& theSegs[4].equals(mySegs[4])) {
					oneWay.add(store.get(0));
					store.remove(i);
					store.remove(0);
					found = true;
					break;
				}
			}
			if (!found) {
				System.out.println("Not Found: " + store.get(0).msg);
				FileWriter errFw = new FileWriter(error, true);
				errFw.write("Not Received: " + store.get(0).Priority
						+ store.get(0).msg);
				errFw.close();
				oneWay.add(store.get(0));
				store.remove(0);
			}
			// break;
		}
		if (oldSize / oneWay.size() == 2)
			System.out.println("No Lost Message!\nCreating one way log...");
		else
			System.out.println("Lost Message(s) Found! See all-error.log!");

		Collections.sort(oneWay);

		// All log in order
		FileWriter inOrderFw = new FileWriter(inOrderLog, true);
		int needLoop = oneWay.size() / 1000;
		// System.out.println("Need Loop: " + needLoop + " Size: " +
		// store.size());
		int count = 0;
		while (needLoop >= count) {
			String filename = outputFolder.getPath() + "//output" + count
					+ ".msc";
			FileWriter fw = new FileWriter(filename, true);
			fw.write("msc{\n\n");
			for (int i = 0; i < ActorNum; i++) {
				if (i == ActorNum - 1) {
					fw.write("Actor" + i);
				} else {
					fw.write("Actor" + i + ",");
				}

			}
			fw.write(";\n");
			for (int i = 1000 * count; i < Math.min(oneWay.size(),
					1000 * (count + 1)); i++) {
				fw.write(oneWay.get(i).msg + "\n");
				inOrderFw.write(oneWay.get(i).Priority + oneWay.get(i).msg
						+ "\n");

			}
			fw.write("}");
			fw.close();
			count++;
		}
		inOrderFw.close();
		System.out.println("In Order Log Created!");
		System.out.println("Generated " + count + " .msc Files!");
		System.out.println("Generating Images...");
		if (makeImg)
			// Create images
			buildImage(count, inputFolder.getPath(), outputFolder.getPath());
		System.out.println("All Processes Done!");
	}

	public static void deleteContent(File out) throws IOException {
		if (out.exists()) {
			FileWriter fw = new FileWriter(out);
			fw.write("");
			fw.close();
		}
	}

	public static void buildImage(int count, String inputFolder,
			String outputFolder) throws Exception {
		for (int i = 0; i < count; i++) {
			ProcessBuilder builder = new ProcessBuilder("cmd.exe", "/c",
					"mscgen -T png -i " + inputFolder + "//output" + i
							+ ".msc -o " + outputFolder + "//output" + i
							+ ".png");
			Process process = builder.start();
			process.waitFor();
			System.out.println((i + 1) + " Image(s) Created!");
		}
	}
}
