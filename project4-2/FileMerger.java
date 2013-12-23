import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

public class FileMerger {

	public static int fileCombine(File out, File folder) {

		File[] listOfFiles = folder.listFiles();
		int fileCount = 0;
		for (int i = 0; i < listOfFiles.length; i++) {
			if (listOfFiles[i].getName().contains(".log")
					&& !listOfFiles[i].getName().contains("all")) {
				fileCount++;
			} else if (listOfFiles[i].getName().contains(".msc")) {
				listOfFiles[i].delete();
			}
		}
		File[] logFiles = new File[fileCount];
		// System.out.println(folder.getPath());
		for (int i = 0; i < fileCount; i++) {
			logFiles[i] = new File(folder.getPath() + "//" + i + ".log");
		}
		mergeFiles(logFiles, out);
		return fileCount;
	}

	public static void mergeFiles(File[] files, File mergedFile) {

		FileWriter fstream = null;
		BufferedWriter out = null;
		try {
			fstream = new FileWriter(mergedFile, true);
			out = new BufferedWriter(fstream);
		} catch (IOException e1) {
			e1.printStackTrace();
		}

		for (File f : files) {
			// System.out.println("Merging: " + f.getName());
			FileInputStream fis;
			try {
				fis = new FileInputStream(f);
				BufferedReader in = new BufferedReader(new InputStreamReader(
						fis));

				String aLine;
				while ((aLine = in.readLine()) != null) {
					out.write(aLine);
					out.newLine();
				}

				in.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		try {
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
}
