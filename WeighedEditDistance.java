import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.UnknownHostException;
import java.util.*;
public class WeighedEditDistance {

	/**
	 * @param args
	 */
	static Hashtable<String,Integer> subcost = new Hashtable<String,Integer>();
	
	public static void main(String[] args) throws UnknownHostException, IOException{
		// TODO Auto-generated method stub
//		subcost.put("as", 1);
//		subcost.put("sa", 1);
        String tacdata_file = "C:\\My Projects\\Zheng - Name Matching\\dist1.csv";
        File tac_f = new File(tacdata_file);
        FileReader tac_f_reader = new FileReader(tac_f);
        BufferedReader buf_tac_reader = new BufferedReader(tac_f_reader);
        String tacline = buf_tac_reader.readLine();
        tacline = buf_tac_reader.readLine();
        String key = "";
        String name = "";
        while(tacline!=null)
        {
        	String[] tmp = tacline.split(",");
        	String[] tmp2 = tmp[1].split(";");
        	if(tmp2.length<2)
        		{
        			key = tmp[0]+""+tmp2[0];
        			subcost.put(key, 1);
        		}
        	else
        		{
        			key = tmp[0] +""+ tmp2[0];
        			subcost.put(key, 1);
        			key = tmp[0] +""+ tmp2[1];
        			subcost.put(key, 1);
        		}
        		        		
        	tacline = buf_tac_reader.readLine();
        }
        buf_tac_reader.close();
        tac_f_reader.close();
                
		WeighedEditDistance wed = new WeighedEditDistance();
//		System.out.println(wed.LD("kris", "krs"));
		
		String name_file = "C:\\My Projects\\Zheng - Name Matching\\surnames.csv";//forenames.csv
		File name_f = new File(name_file);
		FileReader name_f_reader = new FileReader(name_f);
		BufferedReader buf_f_reader = new BufferedReader(name_f_reader);
		tacline = buf_f_reader.readLine();
		String[] tmps = new String[2];
		ArrayList<String> namelist = new ArrayList<String>();
		while(tacline!=null)
		{
			tacline = tacline.replaceAll("\"", "");
			tmps = tacline.split(",");
			namelist.add(tmps[1]);
			name = name+tmps[1]+",";
			tacline = buf_f_reader.readLine();
		}
		buf_f_reader.close();
		name_f_reader.close();
		
		File editdist = new File("C:\\My Projects\\Zheng - Name Matching\\wedit100surname.csv");
		PrintWriter out = new PrintWriter(editdist);
		out.println(name.substring(0, name.length()-1));
		
		System.out.println(namelist.size());
		int[][] wdistance = new int[10000][10000];
		for(int i=0;i<namelist.size();i++)
		{
			name = "";
			for(int j=0;j<namelist.size();j++)
			{
				wdistance[i][j] = wed.LDWeigtDel3(namelist.get(i),namelist.get(j));
				if(wdistance[i][j]>100)
					wdistance[i][j] = 100;
				name = name + wdistance[i][j]+",";
//				System.out.println(namelist.get(i)+":"+namelist.get(j)+"="+wdistance[i][j]);
			}
			out.println(name.substring(0, name.length()-1));
//			System.out.println(name);
		}
		out.flush();
		out.close();
		
//		File editdist = new File("C:\\My Projects\\Zheng - Name Matching\\wedit1to2.csv");
//		PrintWriter out = new PrintWriter(editdist);
//		out.println(name.substring(0, name.length()-1));
//		
//		System.out.println(namelist.size());
//		int[][] wdistance = new int[10000][10000];
//		for(int i=0;i<namelist.size();i++)
//		{
//			name = "";
//			for(int j=0;j<namelist.size();j++)
//			{
//				wdistance[i][j] = wed.LDWeigtDel3(namelist.get(i),namelist.get(j));
//				if(wdistance[i][j]<3&&wdistance[i][j]>0)
//					out.println(namelist.get(i)+","+namelist.get(j)+","+wdistance[i][j]);
//				if(wdistance[i][j]>100)
//					wdistance[i][j] = 100;
//				name = name + wdistance[i][j]+",";
//				System.out.println(namelist.get(i)+":"+namelist.get(j)+"="+wdistance[i][j]);
//			}
//			out.println(name.substring(0, name.length()-1));
//			System.out.println(name);
//		}
		out.flush();
		out.close();
		
		System.out.println(wed.LDWeigtDel3("ian", "susan"));
	}
	
    public int LD (String s, String t) {
        int d[][]; // matrix
        int n; // length of s
        int m; // length of t
        int i; // iterates through s
        int j; // iterates through t
        char s_i; // ith character of s
        char t_j; // jth character of t
        Integer cost; // cost
        Integer delcost;//delcost
        String key ="";
          // Step 1

          n = s.length ();
          m = t.length ();
          if (n == 0) {
            return m;
          }
          if (m == 0) {
            return n;
          }
          d = new int[n+1][m+1];

          // Step 2

          for (i = 0; i <= n; i++) {
            d[i][0] = i;
          }

          for (j = 0; j <= m; j++) {
            d[0][j] = j;
          }

          // Step 3

          for (i = 1; i <= n; i++) {

            s_i = s.charAt (i - 1);

            // Step 4

            for (j = 1; j <= m; j++) {

              t_j = t.charAt (j - 1);

              // Step 5

              if (s_i == t_j) {
                cost = 0;
              }
              else {
            	key = s_i+""+t_j;
                cost = subcost.get(key);
                cost=(cost!=null?cost:100);
              }

              // Step 6

              d[i][j] = Minimum (d[i-1][j]+1, d[i][j-1]+1, d[i-1][j-1] + cost);

            }

          }

          // Step 7

          return d[n][m];

        }
    
    public int LDWeigtDel (String s, String t) {
        int d[][]; // matrix
        int n; // length of s
        int m; // length of t
        int i; // iterates through s
        int j; // iterates through t
        char s_i; // ith character of s
        char t_j; // jth character of t
        Integer cost; // cost
        Integer delcost;//delcost
        String key ="";
          // Step 1

          n = s.length ();
          m = t.length ();
          if (n == 0) {
            return m;
          }
          if (m == 0) {
            return n;
          }
          d = new int[n+1][m+1];

          // Step 2

          for (i = 0; i <= n; i++) {
            d[i][0] = i;
          }

          for (j = 0; j <= m; j++) {
            d[0][j] = j;
          }

          // Step 3

          for (i = 1; i <= n; i++) {

            s_i = s.charAt (i - 1);

            // Step 4

            for (j = 1; j <= m; j++) {

              t_j = t.charAt (j - 1);
              
              // Step 5
              if(i==1)
            	  delcost = 1;
              else
              {
            	  key = s.charAt(i-2)+""+s.charAt(i-1);
            	  delcost = subcost.get(key);
            	  delcost = (delcost!=null?delcost:100);
              }

              if (s_i == t_j) {
                cost = 0;
              }
              else {
            	key = s_i+""+t_j;
                cost = subcost.get(key);
                cost=(cost!=null?cost:100);
              }

              // Step 6

              d[i][j] = Minimum (d[i-1][j]+delcost, d[i][j-1]+1, d[i-1][j-1] + cost);
              System.out.println("for cell "+i+":"+j+" the del, ins, and sub are "+(d[i-1][j]+delcost)+":"+(d[i][j-1]+1)+":"+(d[i-1][j-1] + cost));

            }

          }

          // Step 7
    	  for(j = 0;j<=m;j++){
          	  for (i = 0; i <= n; i++) 
        	  {
        		  System.out.print(d[i][j]+",");
        	  }
        	  System.out.println("");
          }

          return d[n][m];

        }
    
    public int LDWeigtDel3 (String s, String t) {
        int d[][]; // matrix
        int n; // length of s
        int m; // length of t
        int i; // iterates through s
        int j; // iterates through t
        char s_i; // ith character of s
        char t_j; // jth character of t
        Integer cost; // cost
        Integer delcost;//delcost
        Integer inscost;//insertion cost
        String key ="";
          // Step 1

          n = s.length ();
          m = t.length ();
          if (n == 0) {
            return m;
          }
          if (m == 0) {
            return n;
          }
          d = new int[n+1][m+1];

          // Step 2

          for (i = 0; i <= n; i++) {
        	  if(i==0)
        		  d[i][0] = 0;
        	  else if(i==1)
        		  d[i][0] = 1;
        	  else
        	  {
        		  key = s.charAt (i - 2)+""+s.charAt (i-1);
        		  delcost = subcost.get(key);
        		  delcost = (delcost!=null?delcost:100);
        		  d[i][0] = delcost + d[i-1][0];
        	  }
          }

          for (j = 0; j <= m; j++) {
        	  if(j==0)
        		  d[0][j] = 0;
        	  else if(j==1)
        		  d[0][j] = 100;
        	  else
        		  d[0][j] = d[0][j-1] + 100;
//        	  d[0][j] = j;
          }

          // Step 3

          for (i = 1; i <= n; i++) {

              s_i = s.charAt (i - 1);

              // Step 4

              for (j = 1; j <= m; j++) {

                t_j = t.charAt (j - 1);
                
                // Step 5
                if(i==1)
              	  delcost = 1;
                else
                {
              	  key = s.charAt(i-2)+""+s.charAt(i-1);
              	  delcost = subcost.get(key);
              	  delcost = (delcost!=null?delcost:100);
             	  delcost = 1;
                }
                
                if(j==1)
                	inscost = 100;
                else
                {
                	  key = t.charAt(j-2)+""+t.charAt(j-1);
                  	  inscost = subcost.get(key);
                  	  inscost = (inscost!=null?inscost:100);
                }

                if (s_i == t_j) {
                  cost = 0;
                }
                else {
              	key = s_i+""+t_j;
                  cost = subcost.get(key);
                  cost=(cost!=null?cost:100);
                }

                // Step 6

                d[i][j] = Minimum (d[i-1][j]+delcost, d[i][j-1]+inscost, d[i-1][j-1] + cost);//   d[i][j] = Minimum (d[i-1][j]+delcost, d[i][j-1]+1, d[i-1][j-1] + cost)
//                System.out.println("for cell "+i+":"+j+" the del, ins, and sub are "+(d[i-1][j]+delcost)+":"+(d[i][j-1]+1)+":"+(d[i-1][j-1] + cost));

              }

            }

            // Step 7
//      	  for(j = 0;j<=m;j++){
//            	  for (i = 0; i <= n; i++) 
//          	  {
//          		  System.out.print(d[i][j]+",");
//          	  }
//          	  System.out.println("");
//            }

            return d[n][m];

        }
    
    private int Minimum (int a, int b, int c) {
        int mi;

          mi = a;
          if (b < mi) {
            mi = b;
          }
          if (c < mi) {
            mi = c;
          }
          return mi;

        }

}
