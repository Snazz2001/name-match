import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.UnknownHostException;
import java.util.*;

public class WeighteditdistProp {

	/**
	 * @param args
	 */
	public static void main(String[] args)  throws UnknownHostException, IOException{
		// TODO Auto-generated method stub
		int index =10000;
		int max = 0;
        String tacdata_file = "C:\\My Projects\\Zheng - Name Matching\\wedit100surname.csv";
        File tac_f = new File(tacdata_file);
        FileReader tac_f_reader = new FileReader(tac_f);
        BufferedReader buf_tac_reader = new BufferedReader(tac_f_reader);
        String tacline = buf_tac_reader.readLine();
        tacline = buf_tac_reader.readLine();
        int counter = 0;
        String prop = "";
		File editdist = new File("C:\\My Projects\\Zheng - Name Matching\\wedit100surnameprop.csv");
		PrintWriter out = new PrintWriter(editdist);
        String[] tmp = new String[index];
        int[] dists = new int[index];
 
		long startTime = System.currentTimeMillis();

        while(tacline!=null)
        {
        	tmp = tacline.split(",");
        	for(int i=0;i<tmp.length;i++)
        	{
        		dists[i] = Integer.parseInt(tmp[i]);
        	}
        	for(int i=0;i<dists.length;i++)
        	{
 //       		System.out.println("processing "+i+" length is "+dists.length);
        		counter = 0;
        		for(int j=0;j<dists.length;j++)
        		{
        			if(dists[j]<=dists[i])
        			{
        				counter++;
 //       				System.out.println("for dist "+ i+" = "+ dists[i] +">= dist " +j +" = "+ dists[j] + " the counter increase by 1 "+ counter);
        			}
        		}
        		prop = prop+""+counter+",";
  //      		System.out.println(prop);
          	}
        	out.println(prop.substring(0, prop.length()-1));
        	prop = "";
        	tacline = buf_tac_reader.readLine();
        	max++;
        	System.out.println(max);
            }
		long endTime = System.currentTimeMillis();
		long seconds = (endTime-startTime)/1000;
		System.out.println("using "+seconds+" seconds");
		
		out.flush();
		out.close();
		
        buf_tac_reader.close();
        tac_f_reader.close();
	}

}
