import java.io.*;
//import java.nio.file.Files;
import java.util.*;

public class Project4 {
	public static void main(String args[])
	{
		try
		{
			int i, j, nodeid[] = new int[100], logtick=0, to, from, l = 0, r = 0;
			int endtick = 0;
			float s, w;
			String filename = "", str;
			File folder = null;
			
			///////////// checking the arguments
			if(args[0].equals("-r"))
			{
				l = Integer.parseInt(args[1]);
				r =Integer.parseInt(args[2]);
				if(l>r)
				{
					System.out.println("Invalid Arguments");
					System.exit(0);
				}
				File folder1 = new File(System.getProperty("user.dir") + "/log");
				folder = new File(System.getProperty("user.dir") + "/log1");
				
				if(!folder.exists())
				{
					folder.mkdir();
				}

				File[] lof = folder1.listFiles();
				if(args[0].equals("-r"))
				{
					if(l > lof.length || r > lof.length)
					{
						System.out.println("Invalid Arguments");
						System.exit(0);
					}
					for(i=l; i<=r; i++)
					{
						byte[] buf = new byte[4096];
						int len;
						File src = new File(System.getProperty("user.dir")+"/log" + "/Actor"+ i + ".log");
						File dst = new File(System.getProperty("user.dir")+"/log1" + "/Actor"+ i + ".log");
						if(!dst.exists()) { dst.createNewFile();}
						InputStream in = new FileInputStream(src);
						OutputStream out = new FileOutputStream(dst);
						while ((len = in.read(buf)) > 0)
						{
							out.write(buf, 0, len);
						}
						in.close();
						out.close();
					}
				}
			}
			else
			{
				folder = new File(System.getProperty("user.dir") + "/log");
			}
			File[] listOfFiles = folder.listFiles();
			String[] liststr = new String[100];
			BufferedReader[] in = new BufferedReader[100];
			String ex[] = new String[100];
			boolean run = true;
			
			List<message> msg = new ArrayList<message>();
			List<state> st = new ArrayList<state>();
			List<message> rec = new ArrayList<message>();
		
			for(i = 0; i < listOfFiles.length; i++)
			{	
				in[i] = new BufferedReader(new FileReader(listOfFiles[i]));
			}
		
			for(i = 0; i < listOfFiles.length; i++)
			{
				filename = listOfFiles[i].getName();
				if(listOfFiles[i].isFile() && (filename.endsWith(".log")) && (!filename.endsWith("er.log")))
				{
					liststr[i] = in[i].readLine();
					nodeid[i] = Integer.parseInt(liststr[i]);
			
				}
			}
			
			while(run)
			{
				for(i = 0; i < listOfFiles.length; i++)
				{
					filename = listOfFiles[i].getName();
					if(listOfFiles[i].isFile() && (filename.endsWith(".log")) && (!filename.endsWith("er.log")))
					{
						liststr[i] = in[i].readLine();
					
						if(liststr[i] != null)
						{
							StringTokenizer token = new StringTokenizer(liststr[i], " ");
							logtick = Integer.parseInt(token.nextToken());
							if(logtick > endtick)
								endtick = logtick;
							
							token.nextToken();
							str = token.nextToken();
						
							if(str.equals("SEND"))
							{
								token.nextToken();
								str = token.nextToken();
								if(!str.equals("Manager"))
								{
									to = Integer.parseInt(str);
									token.nextToken();
									token.nextToken();
									token.nextToken();
									token.nextToken();
									s = Float.parseFloat(token.nextToken());
									token.nextToken();
									token.nextToken();
									w = Float.parseFloat(token.nextToken());
									msg.add(new message(logtick, nodeid[i], to, s, w));
	
								}
							}
							else if(str.equals("RECEIVE"))
							{
								token.nextToken();
								str = token.nextToken();
								if(!str.equals("Manager"))
								{
									from = Integer.parseInt(str);
									token.nextToken();
									token.nextToken();
									token.nextToken();
									token.nextToken();
									s = Float.parseFloat(token.nextToken());
									token.nextToken();
									token.nextToken();
									w = Float.parseFloat(token.nextToken());
									rec.add(new message(logtick, from, nodeid[i], s, w));
		
								}
							}
							else if(str.equals("STATE"))
							{
								token.nextToken();
								str = token.nextToken();
								if(str.equals("RUNNING"))
								{
									token.nextToken();
									token.nextToken();
									s = Float.parseFloat(token.nextToken());
									token.nextToken();
									token.nextToken();
									w = Float.parseFloat(token.nextToken());
									st.add(new state(nodeid[i], logtick, s, w));
							
								}								
							}
							else if(str.equals("ERROR"))
							{
								//System.out.println("Inside Error!");
								st.add(new state(nodeid[i], logtick, -1, -1));
								token.nextToken();
								while(token.hasMoreTokens())
								{
									ex[i] = ex[i] + token.nextToken();
								}
								
								while(liststr[i]!= null)
								{
									liststr[i] = in[i].readLine();
								}
							}
						}
					}
				}

				run = false;
				for(i = 0; i < listOfFiles.length; i++)
				{
					run = run | (!(liststr[i]== null)); 
				}				
			}
		
			for(i = 0; i < listOfFiles.length; i++)
			{
				in[i].close();
			}
			
			////////////////////////////// create the graphviz file//////////////
			// sort the collections
		    Collections.sort(msg, comp1);
		    Collections.sort(st, comp2);
		    Collections.sort(st, comp3);
		    Collections.sort(rec, comp1);
		    
		    ////////////////////
		    int msgsize = 0;
		    int stsize = 0;
		    int recsize = 0;
		    message m;
		    state stt;
		    msgsize = msg.size();
		    stsize = st.size();
		    recsize = rec.size();
		   
		    
		    //////////// dump it....
		    filename = System.getProperty("user.dir") + "/graph.dot";
		    PrintWriter out = new PrintWriter(new File(filename));
		    
		    if(args[0].equals("-a") || args[0].equals("-r"))
		    {
		    out.println("digraph G {");
		    // the time line...
		   /* str = "{node [shape=plaintext]; 1";
		    for(i=1; i<endtick; i++)
		    	str = str + "->" + (i+1);
		    str = str + " [style = dotted];}";
		    out.println(str);
		    */
		    // create the nodes
		    boolean found = false, color = true;
		    int prevj = -1;
		    float prevw = 0, prevs = 0;
		    stsize = st.size();
		    String str1 = "";

		    for(j = 0; j< listOfFiles.length; j++)
		    {
		    	prevs = nodeid[j]+1;
		    	prevw = 1;
		    	out.println("subgraph cluster" + j + " {color=blue");
		    	str1 = "";
		    	for(i=1; i<=endtick; i++)
		    	{
		    		for(int k=0; k<stsize; k++)
		    		{
		    			stt = st.get(k);
		    			if((stt.nodeid == nodeid[j]) && (stt.tick == i))
		    			{
		    				prevs = stt.s;
		    				prevw = stt.w;
		    				found = true;
		    			}
		    			if((stt.nodeid == nodeid[j]) && (stt.tick > i))
		    				color = false;
		    		}
    				if(prevs == -1 && prevw == -1)
    				{
    					if(prevj != nodeid[j])
    					{
    						str = "A" + nodeid[j] + "0" + i + "[shape=point, label=\""+ nodeid[j] + "| Exception- " + ex[j] + "\", color = red];";
    						prevj = nodeid[j];
    					}
    					else
    					{
    						str = "A" + nodeid[j] + "0" + i + "[shape=point, label=\""+ nodeid[j] + "| -\", color = red];";
    					}
    				}
    				else if(found == true)
		    		{
		    			str = "A" + nodeid[j] + "0" + i + "[shape=point, label=\""+ nodeid[j] + "|{ s=" + prevs + "| w=" + prevw  + "}\"];";
		    			found = false;
		    		}
		    		else
		    		{
		    			if(color == false)
		    			{
		    				str = "A" + nodeid[j] + "0" + i + "[shape=point, label=\""+ nodeid[j] + "|{ s=" + prevs + "| w=" + prevw  + "}\", color = blue];";
		    				color = true;
		    			}
		    			else
		    			{
		    				str = "A" + nodeid[j] + "0" + i + "[shape=point, label=\""+ nodeid[j] + "|{ s=" + prevs + "| w=" + prevw  + "}\", color = green];";
		    			}
		    		}
			    	out.println(str);
			    	if(str1.length() == 0)
			    	{
			    		str1 = "A" + nodeid[j] + "0" + i;
			    	}
			    	else
			    		str1 = str1 + "->A" + nodeid[j] + "0" + i;
		    	}
		    	str1 = str1 + "[style=dotted, arrowType=vee];";
    			out.println(str1);
		    	out.println("label = \"Actor" + nodeid[j] + "\";}");
		    }

    		// //////////////////////// Messages
    		found = false;
    		int lostmsg = 0;
    		
    		msgsize = msg.size();
    		for(i=0; i<msgsize; i++)
		    {
		    	m = msg.get(i);
		    	for(j=0; j<stsize; j++)
        		{
        			stt = st.get(j);
        			if(stt.tick >= (m.tick + 1) && m.to == stt.nodeid)
        			{
        				found = true;
        			}
        		}
		    	if(found == true)
		    	{
		    		out.println("A" + m.from + "0" + m.tick + " -> A" + m.to + "0" + (m.tick+1) + " [label = \"Send:" + m.s + "/" + m.w + "\"];");
		    		found = false;
		    	}
		    	else
		    	{
		    		lostmsg++;
		    		color = true;
		    		for(j=0; j<listOfFiles.length; j++)
		    		{
		    			if(m.to == nodeid[j])
		    				out.println("A" + m.from + "0" + m.tick + " -> A" + m.to + "0" + (m.tick+1) + " [label = \"Send:" + m.s + "/" + m.w + "\", color = red];");
		    		}
		    	}
		    	
		    }
    		
    		

    		///////////////////////// capture lost messages.
		    out.println("{node [shape=plaintext]; \"LOSS = " + lostmsg + "\"}");
		    
		    out.println("}");
			out.close();

		    }
		
    		
    		////////////////////////// Tracing by the Actor.////////////////////////////////////////////////////////////////////////////////////// 
    		////write new loop to create states and messages to dump to .dot file
    		if(args[0].equals("-s"))
    		{
    			int node = Integer.parseInt(args[1]);
    			int nodearr[] = new int[100], cnt =0, prevj=0;
    			float prevs=0, prevw =0;
    			boolean found = false, color = true;
    			String str1 = "";
    		    List<state> stdump = new ArrayList<state>();
    			List<message> msgdump = new ArrayList<message>();
    			
    			for(i=0; i<msgsize; i++)
    			{
    				m = msg.get(i);
    				
    				if(m.from == node || m.to == node)
    					msgdump.add(m);
    			}
    			for(i=0; i<recsize; i++)
    			{
    				m=rec.get(i);
    				if(m.from == node || m.to == node)
    					msgdump.add(m);
    			}
    			
    			msgsize = msgdump.size();
    			
    		    for(i=0; i<stsize; i++)
    		    {
    		    	stt = st.get(i);
    		    	for(j=0; j<msgsize; j++)
    		    	{
    		    		m = msgdump.get(j);
    		    		if(stt.nodeid == m.from || stt.nodeid == m.to)
    		    		{
    		    			stdump.add(stt);
    		    			break;
    		    		}
    		    	}
    		    }
    			stsize = stdump.size();
    			
    			Collections.sort(msgdump, comp1);
    		    Collections.sort(stdump, comp2);
    		    Collections.sort(stdump, comp3);
    		    
    		    cnt = 0;
    		    for(i=0; i<stsize; i++)
    		    {
    		    	stt = stdump.get(i);
    		    	if(cnt == 0 && stt.nodeid != node)
    		    	{
    		    		nodearr[cnt] = stt.nodeid;
    		    		cnt++;
    		    	}
    		    	else if(nodearr[cnt-1] < stt.nodeid && stt.nodeid != node)
    		    	{
    		    		nodearr[cnt] = stt.nodeid;
    		    		cnt++;
    		    	}
    		    }

    		
    		    
    			
    		    out.println("digraph G {");
    		    // the time line...
    		    str = "{node [shape=plaintext]; 1";
    		    for(i=1; i<endtick; i++)
    		    	str = str + "->" + (i+1);
    		    str = str + " [style = dotted];}";
    		    out.println(str);   
    		    
    		    out.println("subgraph cluster" + node + " {");
    		    prevs = node;
    		    prevw =  1;
    		    str1 = "";
    		    for(i=0; i<endtick; i++)
    		    {
    		    	for(j=0; j<stsize; j++)
    		    	{
    		    		stt = stdump.get(j);
    		    		if(stt.nodeid == node && stt.tick == i)
    		    		{
    		    			prevs = stt.s;
    		    			prevw = stt.w;
    		    			found = true;
    		    		}
    		    		if((stt.nodeid == node) && (stt.tick > i))
		    				color = false;
    		    	}
    				if(prevs == -1 && prevw == -1)
    				{
    					if(prevj != node)
    					{
    						str = "A" + node + "0" + i + "[shape=point, label=\""+ node + "| Exception- " + ex[node] + "\", color = red];";
    						prevj = node;
    					}
    					else
    					{
    						str = "A" + node + "0" + i + "[shape=point, label=\""+ node + "| -\", color = red];";
    					}
    				}
    				else if(found == true)
		    		{
		    			str = "A" + node + "0" + i + "[shape=point, label=\""+ node + "|{ s=" + prevs + "| w=" + prevw  + "}\"];";
		    			found = false;
		    		}
		    		else
		    		{
		    			if(color == false)
		    			{
		    				str = "A" + node + "0" + i + "[shape=point, label=\""+ node + "|{ s=" + prevs + "| w=" + prevw  + "}\", color = blue];";
		    				color = true;
		    			}
		    			else
		    			{
		    				str = "A" + node + "0" + i + "[shape=point, label=\""+ node + "|{ s=" + prevs + "| w=" + prevw  + "}\", color = green];";
		    			}
		    		}
			    	out.println(str);
			    	if(str1.length() == 0)
			    	{
			    		str1 = "A" + node + "0" + i;
			    	}
			    	else
			    		str1 = str1 + "->A" + node + "0" + i;
		    	}
		    	str1 = str1 + "[style=dotted, arrowType=vee];";
    			out.println(str1);

    		    out.println("label = \"Actor" + node + "\";}");
    		    
    		    
    		    /////////////////////// other Actors
    		    found = false;
    		    for(int ind=0; ind < cnt; ind ++)
    		    {
    		    	prevs = nodearr[ind];
    		    	prevw = 1;
    		    	str1 = "";
    		    	out.println("subgraph cluster" + nodearr[ind] + " {");
    		    	for(i=0; i<endtick; i++)
    		    	{
    		    		for(j=0; j<stsize; j++)
    		    		{
    		    			stt = stdump.get(j);
    		    			if(stt.nodeid == nodearr[ind] && stt.tick == i)
    		    			{
    		    				prevs = stt.s;
    		    				prevw = stt.w;
    		    				found = true;
    		    			}
    		    		}
    		    		if(found == true)
    		    		{
    		    			str = "A" + nodearr[ind] + "0" + i + "[shape=point, label=\""+ nodearr[ind] + "|{ s=" + prevs + "| w=" + prevw  + "}\", style = dotted];";
		    				found = false;
    		    		}
    		    		else
    		    		{
    		    			str = "A" + nodearr[ind] + "0" + i + "[shape=point, label=\""+ nodearr[ind] + "|{ s=" + prevs + "| w=" + prevw  + "}\", style = dotted];";
    		    		}
    		    		out.println(str);
    			    	if(str1.length() == 0)
    			    	{
    			    		str1 = "A" + nodearr[ind] + "0" + i;
    			    	}
    			    	else
    			    		str1 = str1 + "->A" + nodearr[ind] + "0" + i;
    		    	}
    		    	str1 = str1 + "[style=dotted, arrowType=vee];";
        			out.println(str1);
    		    	out.println("label = \"Actor" + nodearr[ind] + "\";}");
    		    }
    		    
    		    ////////////////////////// Messages //////////////////////
    		    found = false;
        		int lostmsg = 0;
        		nodearr[cnt] = node;
        		cnt++;
        		
        		for(i=0; i<msgsize; i++)
    		    {
    		    	m = msgdump.get(i);
    		    	for(j=0; j<stsize; j++)
            		{
            			stt = stdump.get(j);
            			if(stt.tick >= (m.tick + 1) && m.to == stt.nodeid)
            			{
            				found = true;
            			}
            		}
    		    	if(found == true)
    		    	{
    		    		out.println("A" + m.from + "0" + m.tick + " -> A" + m.to + "0" + (m.tick+1) + " [label = \"Send:" + m.s + "/" +  m.w + "\"];");
    		    		found = false;
    		    	}
    		    	else
    		    	{
    		    		lostmsg++;
    		    		color = true;
    		    		for(j=0; j<cnt; j++)
    		    		{
    		    			if(m.to == nodearr[j])
    		    				out.println("A" + m.from + "0" + m.tick + " -> A" + m.to + "0" + (m.tick+1) + " [label = \"Send:" +  m.s + "/" +  m.w + "\", color = red];");
    		    		}
    		    	}
    		    	
    		    }

        		out.println("{node [shape=plaintext]; " + endtick + "-> \"Loss = " + lostmsg + "\"}");

    		    out.println("}");
    			out.close();

    		}
		}
		catch(Exception e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace(System.out);
		}
	}
	
	public static Comparator<message> comp1 = new Comparator<message>()
	{
		public int compare(message m, message n)
        {
            return m.tick - n.tick;
        }
	};

	public static Comparator<state> comp2 = new Comparator<state>()
	{
		public int compare(state m, state n)
	       {
	           return m.tick - n.tick;
	       }
	};
	
	public static Comparator<state> comp3 = new Comparator<state>()
	{
		public int compare(state m, state n)
	       {
	           return m.nodeid - n.nodeid;
	       }
	};
}

class message
{
	public int tick, from, to;
	public float s,w;
	
	public message(int tick, int from, int to, float s, float w)
	{
		this.tick = tick;
		this.from = from;
		this.to = to;
		this.s = s;
		this.w = w;
	}
}

class state
{
	public int nodeid, tick;
	public float s, w;
	
	public state(int nodeid, int tick, float s, float w)
	{
		this.nodeid = nodeid;
		this.tick = tick;
		this.s = s;
		this.w = w;
	}
}
