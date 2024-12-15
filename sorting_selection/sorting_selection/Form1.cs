using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace sorting_selection_el
{

    public partial class Form1 : Form
    {
        string path = ".";
        string image1 = "";
        string image2 = "";
        int image1_pos = 0;
        int image2_pos = 0;

        sorting_selection_el.Form2 form2 = null;

        struct Employee
        {
            public int id;
            public double monotonicity;
            public string feature;
            public int lookback;
            public int lookback_slide;
            public int smooth_window;
            public int smooth_window_slide;
            public int smooth_window2;
            public int smooth_window_slide2;
            public int sigin;
            public double max;
            public double min;
            public string image;
            public string filename_r;
            public double rmse12;
            public double Confidence;
            public string Type;
        }


        public Form1()
        {
            InitializeComponent();
            string[] cmds = System.Environment.GetCommandLineArgs();
            if (cmds.Length > 1)
            {
                path = cmds[1];
            }
            else
            {
                path = "..\\images";
            }
            bool s1 = File.Exists(path + "\\feature_summarys_best.csv");
            bool s2 = File.Exists(path + "\\feature_summarys.csv");
            if (s1 && s2)
            {
                button1_Click(null, null);
            }

        }


        static List<Employee> ReadCsvFile(string filePath, bool best = false)
        {
            List<Employee> employees = new List<Employee>();
            //Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
            var lines = File.ReadAllLines(filePath, Encoding.GetEncoding("shift_jis"));

            int count = 0;
            foreach (var line in lines)
            {
                count++;
                if (count == 1) continue;
                var values = line.Split(',');
                var employee = new Employee()
                {
                    id = int.Parse(values[0]),
                    monotonicity = double.Parse(values[1]),
                    feature = values[2],
                    lookback = int.Parse(values[3]),
                    lookback_slide = int.Parse(values[4]),
                    smooth_window = int.Parse(values[5]),
                    smooth_window_slide = int.Parse(values[6]),
                    smooth_window2 = int.Parse(values[7]),
                    smooth_window_slide2 = int.Parse(values[8]),
                    sigin = int.Parse(values[9]),
                    max = double.Parse(values[10]),
                    min = double.Parse(values[11]),
                    image = values[12],
                    filename_r = values[13],
                    rmse12 = double.Parse(values[14]),
                    Confidence = 0.0,
                    Type = ""
                };
                if (best)
                {
                    employee.Confidence = double.Parse(values[14]);
                    employee.Type = values[15];
                }
                employees.Add(employee);
            }

            return employees;
        }

        public  System.Drawing.Image CreateImage(string filename)
        {
            System.IO.FileStream fs = new System.IO.FileStream(
                filename,
                System.IO.FileMode.Open,
                System.IO.FileAccess.Read);
            System.Drawing.Image img = System.Drawing.Image.FromStream(fs);
            fs.Close();
            return img;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            bool s1 = File.Exists(path + "\\feature_summarys_best.csv");
            bool s2 = File.Exists(path + "\\feature_summarys.csv");
            if (!s1) return;
            if (!s2) return;

            List<Employee> csv = ReadCsvFile(path + "\\feature_summarys.csv");
            List<Employee> csv2 = ReadCsvFile(path + "\\feature_summarys_best.csv", true);

            listBox1.Items.Clear();
            listBox2.Items.Clear();
            listBox3.Items.Clear();
            listBox4.Items.Clear();
            int n = 0;
            int N = csv2.Count;
            trackBar1.Maximum = N;

            double p = 0.8;

            if (false)
            {
                for (int k = 0; k < N; k++)
                {
                    if (csv2[k].Confidence < p) continue;
                    n++;
                }
                if (n == 0)
                {
                    p = 0.7;
                    for (int k = 0; k < N; k++)
                    {
                        if (csv2[k].Confidence < p) continue;
                        n++;
                    }
                }
                if (n == 0)
                {
                    p = 0.6;
                    for (int k = 0; k < N; k++)
                    {
                        if (csv2[k].Confidence < p) continue;
                        n++;
                    }
                }
            }

            n = 0;
            for (int k = 0; k < N; k++)
            {
                string fileName = csv2[k].filename_r;
                double Confidence = csv2[k].Confidence;
                double monotonicity = csv2[k].monotonicity;
                double ymax = csv2[k].max;
                if (Math.Abs(csv2[k].monotonicity) < 0.2) continue;
                if (csv2[k].rmse12 >= 999999.0) break;

                for (int i = 0; i < csv.Count; i++)
                {
                    string s = csv[i].filename_r;
                    if (s == fileName)
                    {
                        listBox1.Items.Add(path + "\\" + fileName.Replace("\"", "") + ".png");
                        listBox2.Items.Add(Confidence.ToString() + "% : " + csv2[k].Type);
                        listBox3.Items.Add("monotonicity:" + monotonicity.ToString());
                        listBox4.Items.Add("ymax:" + ymax.ToString());
                        pictureBox1.Image = CreateImage(path + "\\" + fileName.Replace("\"", "") + ".png");

                        string t = path + "\\" + fileName.Replace("\"", "") + ".r";

                        string[] delimiter = { "_feature" };
                        string[] tmp = fileName.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);

                        textBox1.Text = "#" + tmp[1] + "\r\n";
                        textBox1.Text += "#" + Confidence.ToString() + "% : " + csv2[k].Type + "\r\n";
                        textBox1.Text += "#monotonicity:" + monotonicity.ToString() + "\r\n\r\n"; ;
                        textBox1.Text += "#ymax:" + ymax.ToString() + "\r\n\r\n"; ;
                        using (StreamReader sr = new StreamReader(t))
                        {
                            textBox1.Text += sr.ReadToEnd();
                        }
                        trackBar1.Value = k;
                        break;
                    }
                }
            }
            trackBar1.Value = 0;
            trackBar1_Scroll(null, null);
        }

        public void trackBar1_Scroll(object sender, EventArgs e)
        {
            if (trackBar1.Value < listBox1.Items.Count)
            {
                string filePath = listBox1.Items[trackBar1.Value].ToString();
                pictureBox1.Image = CreateImage(filePath);
                pictureBox1.Refresh();

                string t = System.IO.Path.ChangeExtension(filePath, ".r");

                string[] delimiter = { "_feature" };
                string[] tmp = filePath.Replace(".png", "").Split(delimiter, StringSplitOptions.RemoveEmptyEntries);

                textBox1.Text = "#" + tmp[1] + "\r\n";
                textBox1.Text += "#" + listBox2.Items[trackBar1.Value].ToString() + "\r\n";
                textBox1.Text += "#" + listBox3.Items[trackBar1.Value].ToString() + "\r\n\r\n";
                textBox1.Text += "#" + listBox4.Items[trackBar1.Value].ToString() + "\r\n\r\n";
                using (StreamReader sr = new StreamReader(t))
                {
                    textBox1.Text += sr.ReadToEnd();
                }
                textBox1.Refresh();
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            trackBar1.Value = Math.Max(0, trackBar1.Value - 1);
            trackBar1_Scroll(sender, e);
        }

        private void button3_Click(object sender, EventArgs e)
        {
            trackBar1.Value = Math.Min(trackBar1.Maximum, trackBar1.Value + 1);
            trackBar1_Scroll(sender, e);
        }

        private void button4_Click(object sender, EventArgs e)
        {
            if (trackBar1.Value < listBox1.Items.Count)
            {
                if (image1 == "")
                {
                    MessageBox.Show("Please select one.");
                    return;
                }
                trackBar1.Value = image1_pos;
                string filePath = listBox1.Items[trackBar1.Value].ToString();
                pictureBox1.Image = CreateImage(filePath);
                pictureBox1.Refresh();

                string t = System.IO.Path.ChangeExtension(filePath, ".r");

                string[] delimiter = { " = " };

                string file = "feature_discovery_output.txt";
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, System.Text.Encoding.GetEncoding("shift_jis")))
                {
                    string[] delimiter2 = { "_feature" };
                    string[] tmp2 = filePath.Replace(".png", "").Split(delimiter2, StringSplitOptions.RemoveEmptyEntries);
                    //sw.Write(tmp2[1].Replace("(", "").Replace(")", "")+"\n");

                    if (image1 != "")
                    {
                        tmp2 = image1.Replace(".png", "").Split(delimiter2, StringSplitOptions.RemoveEmptyEntries);

                        string s = tmp2[1].Replace("(", "").Replace(")", "");
                        sw.Write(s+ "\n");
                    }
                    if (image2 != "")
                    {
                        tmp2 = image2.Replace(".png", "").Split(delimiter2, StringSplitOptions.RemoveEmptyEntries);
                        sw.Write(tmp2[1].Replace("(", "").Replace(")", "") + "\n");
                    }else
                    {
                        tmp2 = image1.Replace(".png", "").Split(delimiter2, StringSplitOptions.RemoveEmptyEntries);
                        string s = tmp2[1].Replace("(", "").Replace(")", "");
                        string[] delimiter3 = { "." };
                        s = s.Split(delimiter3, StringSplitOptions.RemoveEmptyEntries)[0];
                        sw.Write(s + "..\n");
                    }
                    using (StreamReader sr = new StreamReader(t))
                    {
                        while (true)
                        {
                            string line = sr.ReadLine();
                            if (line == null) break;

                            if (line.Contains("lookback = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox10," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            if (line.Contains("lookback_slide = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox11," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            if (line.Contains("smooth_window = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox5," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            if (line.Contains("smooth_window_slide = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox6," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            if (line.Contains("smooth_window2 = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox13," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            if (line.Contains("smooth_window_slide2 = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox12," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            if (line.Contains("threshold_target = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                sw.Write("textBox21," + tmp[1].Replace("\r\n", "") + "\n");
                            }
                            // .Text
                            if (line.Contains("sigin = "))
                            {
                                string[] tmp = line.Split(delimiter, StringSplitOptions.RemoveEmptyEntries);
                                if (int.Parse(tmp[1].Replace("\r\n", "")) > 0)
                                {
                                    sw.Write("comboBox3,+\n");
                                }
                                else
                                {
                                    sw.Write("comboBox3,-\n");
                                }
                            }
                        }
                    }
                }
            }
        }

        private void button5_Click(object sender, EventArgs e)
        {
            if (image2 == "" && image1 == "")
            {
                if (trackBar1.Value < listBox1.Items.Count)
                {
                    string filePath = listBox1.Items[trackBar1.Value].ToString();
                    pictureBox2.Image = CreateImage(filePath);
                    pictureBox2.Refresh();
                    image1 = filePath;
                    image1_pos = trackBar1.Value;
                }
            }
            else
            {
                if (image1 != "" && image2 == "")
                {
                    if (trackBar1.Value < listBox1.Items.Count)
                    {
                        string filePath = listBox1.Items[trackBar1.Value].ToString();
                        pictureBox3.Image = CreateImage(filePath);
                        pictureBox3.Refresh();
                        image2 = filePath;
                        image2_pos = trackBar1.Value;
                    }
                }
            }
        }

        private void pictureBox2_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            pictureBox2.Image = null;
            pictureBox2.Refresh();
            image1 = "";
            if ( image2 != "")
            {
                pictureBox2.Image = pictureBox3.Image;
                pictureBox2.Refresh();
                image1 = image2;
                image1_pos = image2_pos;
                trackBar1.Value = image1_pos;
                trackBar1_Scroll(sender, e);

                pictureBox3.Image = null;
                pictureBox3.Refresh();
                image2 = "";
                image2_pos = 0;
            }
        }

        private void pictureBox3_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            pictureBox3.Image = null;
            pictureBox3.Refresh();
            image2 = "";
            image2_pos = 0;
        }

        private void pictureBox2_MouseClick(object sender, MouseEventArgs e)
        {
            if (image1 != "")
            {
                trackBar1.Value = image1_pos;
                trackBar1_Scroll(sender, e);
            }
        }

        private void pictureBox3_MouseClick(object sender, MouseEventArgs e)
        {
            if (image2 != "")
            {
                trackBar1.Value = image2_pos;
                trackBar1_Scroll(sender, e);
            }
        }

        private void toolStrip1_ItemClicked(object sender, ToolStripItemClickedEventArgs e)
        {

        }

        private void button6_Click(object sender, EventArgs e)
        {
            form2 = new sorting_selection_el.Form2();
            form2.listBox1 = listBox1;
            form2.form1 = this;

            form2.View();
        }
    }
}
