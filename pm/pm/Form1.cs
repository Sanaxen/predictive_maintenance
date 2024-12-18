using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace pm
{
    public partial class Form1 : Form
    {
        [DllImport("shlwapi.dll",
            CharSet = CharSet.Auto)]
        private static extern IntPtr PathCombine(
            [Out] StringBuilder lpszDest,
            string lpszDir,
            string lpszFile);

        public string exePath = "";
        public string RlibPath = "";
        public string encoding = "sjis";
        public string base_dir;
        public string work_dir;
        public string csv_file;
        public string csv_dir;
        public string base_name = "";
        public string base_name0 = "";

        public int exist_number = 1;
        public int status = -1;
        System.Diagnostics.Stopwatch stopwatch = new System.Diagnostics.Stopwatch();

        public List<string> imageFiles = null;
        public int start_index = 1;
        public int max_image_limit = 100000;

        public int num_image = 0;
        public int step_image = 1;
        public int max_image = 0;
        public bool animation_stop = false;
        public int output_idx = 0;

        public string imagePictureBox1 = "";
        public string imagePictureBox2 = "";
        public string imagePictureBox3 = "";
        public string imagePictureBox4 = "";
        public string imagePictureBox5 = "";
        public string imagePictureBox6 = "";

        public string htmlPictureBox1 = "";
        public string htmlPictureBox2 = "";
        public string htmlPictureBox3 = "";
        public string htmlPictureBox4 = "";
        public string htmlPictureBox5 = "";
        public string htmlPictureBox6 = "";

        public bool LanguageChangeMessageOff = false;

        public Form1()
        {
            System.Threading.Thread.CurrentThread.CurrentUICulture = new System.Globalization.CultureInfo("en-US");
            exePath = AppDomain.CurrentDomain.BaseDirectory;

            string Language = "en-US";
            if (File.Exists(exePath + "Language.txt"))
            {
                using (StreamReader sr = new StreamReader(exePath + "Language.txt"))
                {
                    Language = sr.ReadToEnd().Replace("\n", "");
                }
            }
            if (Language == "ja-JP")
            {
                System.Threading.Thread.CurrentThread.CurrentUICulture = new System.Globalization.CultureInfo("ja-JP");
            }
            InitializeComponent();

            if (File.Exists(exePath + "R_install_path.txt"))
            {
                using (StreamReader sr = new StreamReader(exePath + "R_install_path.txt"))
                {
                    textBox1.Text = sr.ReadToEnd().Replace("\n", "");
                }
            }

            LanguageChangeMessageOff = true;
            comboBox6.Text = Language;
            LanguageChangeMessageOff = false;

            StringBuilder sb = new StringBuilder(2048);
            IntPtr res = PathCombine(sb, exePath, "..\\..\\..\\..\\library");
            if (res == IntPtr.Zero)
            {
                MessageBox.Show("Failed to obtain absolute path of R library.");
            }
            else
            {
                RlibPath = sb.ToString().Replace("\\", "/");
            }
        }
        public static System.Drawing.Image CreateImage(string filename)
        {
            if (filename == "") return null;
            System.IO.FileStream fs = new System.IO.FileStream(
                filename,
                System.IO.FileMode.Open,
                System.IO.FileAccess.Read);
            System.Drawing.Image img = System.Drawing.Image.FromStream(fs);
            fs.Close();
            return img;
        }

        void save()
        {
            string file = csv_dir + "\\pm_setting_" + base_name0 + string.Format("{0}", output_idx) + ".txt";

            var encoding = new System.Text.UTF8Encoding(false);

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(output_idx.ToString() + "\n");
                    sw.Write(listBox1.Items.Count.ToString() + "\n");
                    for (int i = 0; i < listBox1.Items.Count; i++)
                    {
                        sw.Write(listBox1.Items[i].ToString() + "\n");
                    }

                    sw.Write(listBox1.SelectedItems.Count.ToString() + "\n");
                    if (listBox1.SelectedItems.Count >= 1)
                    {
                        for (int i = 0; i < listBox1.SelectedItems.Count; i++)
                        {
                            sw.Write(listBox1.SelectedIndices[i].ToString() + "\n");
                        }
                    }

                    sw.Write(listBox2.Items.Count.ToString() + "\n");
                    for (int i = 0; i < listBox2.Items.Count; i++)
                    {
                        sw.Write(listBox2.Items[i].ToString() + "\n");
                    }
                    sw.Write(listBox2.SelectedItems.Count.ToString() + "\n");
                    if (listBox2.SelectedItems.Count >= 1)
                    {
                        for (int i = 0; i < listBox2.SelectedItems.Count; i++)
                        {
                            sw.Write(listBox2.SelectedIndices[i].ToString() + "\n");
                        }
                    }


                    sw.Write(listBox3.Items.Count.ToString() + "\n");
                    for (int i = 0; i < listBox3.Items.Count; i++)
                    {
                        sw.Write(listBox3.Items[i].ToString() + "\n");
                    }
                    sw.Write(listBox3.SelectedItems.Count.ToString() + "\n");
                    if (listBox3.SelectedItems.Count >= 1)
                    {
                        for (int i = 0; i < listBox3.SelectedItems.Count; i++)
                        {
                            sw.Write(listBox3.SelectedIndices[i].ToString() + "\n");
                        }
                    }



                    sw.Write("textBox2," + textBox2.Text + "\n");
                    sw.Write("textBox3," + textBox3.Text + "\n");
                    sw.Write("textBox4," + textBox4.Text + "\n");
                    sw.Write("textBox5," + textBox5.Text + "\n");
                    sw.Write("textBox6," + textBox6.Text + "\n");
                    sw.Write("textBox7," + textBox7.Text + "\n");
                    sw.Write("textBox8," + textBox8.Text + "\n");
                    sw.Write("textBox9," + textBox9.Text + "\n");
                    sw.Write("textBox10," + textBox10.Text + "\n");
                    sw.Write("textBox11," + textBox11.Text + "\n");
                    sw.Write("textBox12," + textBox12.Text + "\n");
                    sw.Write("textBox13," + textBox13.Text + "\n");
                    sw.Write("textBox14," + textBox14.Text + "\n");
                    sw.Write("textBox15," + textBox15.Text + "\n");
                    sw.Write("textBox16," + textBox16.Text + "\n");
                    sw.Write("textBox17," + textBox17.Text + "\n");
                    sw.Write("textBox18," + textBox18.Text + "\n");
                    sw.Write("textBox19," + textBox19.Text + "\n");
                    sw.Write("textBox20," + textBox20.Text + "\n");
                    sw.Write("textBox21," + textBox21.Text + "\n");
                    //sw.Write("textBox22," + textBox22.Text + "\n");
                    //sw.Write("textBox23," + textBox23.Text + "\n");
                    //sw.Write("textBox24," + textBox24.Text + "\n");
                    //sw.Write("textBox25," + textBox25.Text + "\n");

                    sw.Write("comboBox1," + comboBox1.Text + "\n");
                    sw.Write("comboBox2," + comboBox2.Text + "\n");
                    sw.Write("comboBox3," + comboBox3.Text + "\n");
                    sw.Write("comboBox4," + comboBox4.Text + "\n");
                    sw.Write("comboBox5," + comboBox5.Text + "\n");
                    //sw.Write("comboBox6," + comboBox6.Text + "\n");
                    //sw.Write("comboBox7," + comboBox7.Text + "\n");
                    //sw.Write("comboBox8," + comboBox8.Text + "\n");
                    //sw.Write("comboBox9," + comboBox9.Text + "\n");
                    //sw.Write("comboBox10," + comboBox10.Text + "\n");

                    sw.Write("numericUpDown1," + numericUpDown1.Value.ToString() + "\n");
                    //sw.Write("numericUpDown2," + numericUpDown2.Value.ToString() + "\n");
                    //sw.Write("numericUpDown3," + numericUpDown3.Value.ToString() + "\n");
                    //sw.Write("numericUpDown4," + numericUpDown4.Value.ToString() + "\n");
                    //sw.Write("numericUpDown5," + numericUpDown5.Value.ToString() + "\n");
                    //sw.Write("numericUpDown6," + numericUpDown6.Value.ToString() + "\n");
                    //sw.Write("numericUpDown7," + numericUpDown7.Value.ToString() + "\n");
                    //sw.Write("numericUpDown8," + numericUpDown8.Value.ToString() + "\n");
                    //sw.Write("numericUpDown9," + numericUpDown9.Value.ToString() + "\n");
                    //sw.Write("numericUpDown10," + numericUpDown10.Value.ToString() + "\n");
                    //sw.Write("numericUpDown11," + numericUpDown11.Value.ToString() + "\n");
                    //sw.Write("numericUpDown12," + numericUpDown12.Value.ToString() + "\n");
                    //sw.Write("numericUpDown13," + numericUpDown13.Value.ToString() + "\n");
                    //sw.Write("numericUpDown14," + numericUpDown14.Value.ToString() + "\n");
                    //sw.Write("numericUpDown15," + numericUpDown15.Value.ToString() + "\n");
                    //sw.Write("numericUpDown16," + numericUpDown16.Value.ToString() + "\n");
                    //sw.Write("numericUpDown17," + numericUpDown17.Value.ToString() + "\n");

                    sw.Write("checkBox1," + (checkBox1.Checked ? "TRUE" : "FALSE") + "\n");
                    sw.Write("checkBox2," + (checkBox2.Checked ? "TRUE" : "FALSE") + "\n");
                    sw.Write("checkBox3," + (checkBox3.Checked ? "TRUE" : "FALSE") + "\n");
                    sw.Write("checkBox4," + (checkBox4.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox5," + (checkBox5.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox6," + (checkBox6.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox7," + (checkBox7.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox8," + (checkBox8.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox9," + (checkBox9.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox10," + (checkBox10.Checked ? "TRUE" : "FALSE") + "\n");
                    //sw.Write("checkBox11," + (checkBox11.Checked ? "TRUE" : "FALSE") + "\n");

                    sw.Write("radioButton1," + (radioButton1.Checked ? "TRUE" : "FALSE") + "\n");
                    sw.Write("radioButton2," + (radioButton2.Checked ? "TRUE" : "FALSE") + "\n");
                    sw.Write("radioButton3," + (radioButton3.Checked ? "TRUE" : "FALSE") + "\n");

                    sw.Write("imagePictureBox1," + imagePictureBox1 + "\n");
                    sw.Write("imagePictureBox2," + imagePictureBox2 + "\n");
                    sw.Write("imagePictureBox3," + imagePictureBox3 + "\n");
                    sw.Write("imagePictureBox4," + imagePictureBox4 + "\n");
                    sw.Write("imagePictureBox5," + imagePictureBox5 + "\n");
                    sw.Write("imagePictureBox6," + imagePictureBox6 + "\n");
                    //sw.Write("imagePictureBox7," + imagePictureBox7 + "\n");
                    //sw.Write("imagePictureBox8," + imagePictureBox8 + "\n");

                    sw.Write("htmlPictureBox1," + htmlPictureBox1 + "\n");
                    sw.Write("htmlPictureBox2," + htmlPictureBox2 + "\n");
                    sw.Write("htmlPictureBox3," + htmlPictureBox3 + "\n");
                    sw.Write("htmlPictureBox4," + htmlPictureBox4 + "\n");
                    sw.Write("htmlPictureBox5," + htmlPictureBox5 + "\n");
                    sw.Write("htmlPictureBox6," + htmlPictureBox6 + "\n");

                    sw.Write("r_path," + textBox1.Text + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }
            try
            {
                File.Copy("pm_setting_" + base_name0 + ".txt", "pre_setting.txt", true);
            }
            catch { }
        }
        void load(string setting_file)
        {
            string file = csv_dir + "\\pm_setting_" + base_name0 + string.Format("{0}", output_idx) + ".txt";

            if (setting_file == "")
            {
                if (base_name0 == "")
                {
                    status = -1;
                    MessageBox.Show("input csv file !");
                    return;
                }
                if (!File.Exists(file)) save();

                if (!File.Exists(file))
                {
                    MessageBox.Show("file not found[" + file + ".txt]");
                }
                listBox1.Items.Clear();
                listBox2.Items.Clear();
                listBox3.Items.Clear();
                //listBox4.Items.Clear();
            }
            else
            {
                file = setting_file;
            }

            string rexe1 = textBox1.Text + "\\x64\\Rscript.exe";
            string rexe = rexe1;

            var encoding = new System.Text.UTF8Encoding(false);

            bool rpath_chg = false;
            System.IO.StreamReader sr = new System.IO.StreamReader(file, encoding);
            if (sr != null)
            {
                while (sr.EndOfStream == false)
                {
                    string s = sr.ReadLine();
                    output_idx = int.Parse(s.Replace("\n", ""));

                    s = sr.ReadLine();
                    int n = int.Parse(s.Replace("\n", ""));
                    for (int i = 0; i < n; i++)
                    {
                        s = sr.ReadLine();
                        if (setting_file == "")
                        {
                            listBox1.Items.Add(s.Replace("\n", ""));
                        }
                    }
                    s = sr.ReadLine();
                    n = int.Parse(s.Replace("\n", ""));
                    for (int i = 0; i < n; i++)
                    {
                        s = sr.ReadLine();
                        int k = int.Parse(s.Replace("\n", ""));
                        listBox1.SetSelected(k, true);
                    }

                    s = sr.ReadLine();
                    n = int.Parse(s.Replace("\n", ""));
                    for (int i = 0; i < n; i++)
                    {
                        s = sr.ReadLine();
                        if (setting_file == "")
                        {
                            listBox2.Items.Add(s.Replace("\n", ""));
                        }
                    }
                    s = sr.ReadLine();
                    n = int.Parse(s.Replace("\n", ""));
                    for (int i = 0; i < n; i++)
                    {
                        s = sr.ReadLine();
                        int k = int.Parse(s.Replace("\n", ""));
                        listBox2.SetSelected(k, true);
                    }


                    s = sr.ReadLine();
                    n = int.Parse(s.Replace("\n", ""));
                    for (int i = 0; i < n; i++)
                    {
                        s = sr.ReadLine();
                        if (setting_file == "")
                        {
                            listBox3.Items.Add(s.Replace("\n", ""));
                        }
                    }
                    s = sr.ReadLine();
                    n = int.Parse(s.Replace("\n", ""));
                    for (int i = 0; i < n; i++)
                    {
                        s = sr.ReadLine();
                        int k = int.Parse(s.Replace("\n", ""));
                        listBox3.SetSelected(k, true);
                    }


                    //s = sr.ReadLine();
                    //n = int.Parse(s.Replace("\n", ""));
                    //for (int i = 0; i < n; i++)
                    //{
                    //    s = sr.ReadLine();
                    //    if (setting_file == "")
                    //    {
                    //        listBox4.Items.Add(s.Replace("\n", ""));
                    //    }
                    //}

                    //s = sr.ReadLine();
                    //n = int.Parse(s.Replace("\n", ""));
                    //for (int i = 0; i < n; i++)
                    //{
                    //    s = sr.ReadLine();
                    //    int k = int.Parse(s.Replace("\n", ""));
                    //    listBox4.SetSelected(k, true);
                    //}
                    while (sr.EndOfStream == false)
                    {
                        s = sr.ReadLine();
                        var ss = s.Split(',');


                        if (ss[0].IndexOf("textBox11") >= 0)
                        {
                            textBox11.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox12") >= 0)
                        {
                            textBox12.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox13") >= 0)
                        {
                            textBox13.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox14") >= 0)
                        {
                            textBox14.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox15") >= 0)
                        {
                            textBox15.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox16") >= 0)
                        {
                            textBox16.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox17") >= 0)
                        {
                            textBox17.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox18") >= 0)
                        {
                            textBox18.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox19") >= 0)
                        {
                            textBox19.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox20") >= 0)
                        {
                            textBox20.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox21") >= 0)
                        {
                            textBox21.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        //if (ss[0].IndexOf("textBox21") >= 0)
                        //{
                        //    textBox21.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("textBox22") >= 0)
                        //{
                        //    textBox22.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("textBox23") >= 0)
                        //{
                        //    textBox23.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("textBox24") >= 0)
                        //{
                        //    textBox24.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("textBox25") >= 0)
                        //{
                        //    textBox25.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        if (ss[0].IndexOf("textBox2") >= 0)
                        {
                            textBox2.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox3") >= 0)
                        {
                            textBox3.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox4") >= 0)
                        {
                            textBox4.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox5") >= 0)
                        {
                            textBox5.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox6") >= 0)
                        {
                            textBox6.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox7") >= 0)
                        {
                            textBox7.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox8") >= 0)
                        {
                            textBox8.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox9") >= 0)
                        {
                            textBox9.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox10") >= 0)
                        {
                            textBox10.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }

                        //if (ss[0].IndexOf("comboBox10") >= 0)
                        //{
                        //    comboBox10.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        if (ss[0].IndexOf("comboBox1") >= 0)
                        {
                            comboBox1.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("comboBox2") >= 0)
                        {
                            comboBox2.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("comboBox3") >= 0)
                        {
                            comboBox3.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("comboBox4") >= 0)
                        {
                            comboBox4.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("comboBox5") >= 0)
                        {
                            comboBox5.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        //if (ss[0].IndexOf("comboBox6") >= 0)
                        //{
                        //    comboBox6.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("comboBox7") >= 0)
                        //{
                        //    comboBox7.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("comboBox8") >= 0)
                        //{
                        //    comboBox8.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("comboBox9") >= 0)
                        //{
                        //    comboBox9.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}

                        //if (ss[0].IndexOf("numericUpDown10") >= 0)
                        //{
                        //    numericUpDown10.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown11") >= 0)
                        //{
                        //    numericUpDown11.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown12") >= 0)
                        //{
                        //    numericUpDown12.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown13") >= 0)
                        //{
                        //    numericUpDown13.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown14") >= 0)
                        //{
                        //    numericUpDown14.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown15") >= 0)
                        //{
                        //    numericUpDown15.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown16") >= 0)
                        //{
                        //    numericUpDown16.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown17") >= 0)
                        //{
                        //    numericUpDown17.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown18") >= 0)
                        //{
                        //    numericUpDown18.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown19") >= 0)
                        //{
                        //    numericUpDown19.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}


                        if (ss[0].IndexOf("numericUpDown1") >= 0)
                        {
                            numericUpDown1.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        //if (ss[0].IndexOf("numericUpDown2") >= 0)
                        //{
                        //    numericUpDown2.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown3") >= 0)
                        //{
                        //    numericUpDown3.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown4") >= 0)
                        //{
                        //    numericUpDown4.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown5") >= 0)
                        //{
                        //    numericUpDown5.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown6") >= 0)
                        //{
                        //    numericUpDown6.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown7") >= 0)
                        //{
                        //    numericUpDown7.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown8") >= 0)
                        //{
                        //    numericUpDown8.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("numericUpDown9") >= 0)
                        //{
                        //    numericUpDown9.Text = ss[1].Replace("\r\n", "");
                        //    continue;
                        //}

                        //if (ss[0].IndexOf("checkBox11") >= 0)
                        //{
                        //    checkBox11.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("checkBox10") >= 0)
                        //{
                        //    checkBox10.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}
                        if (ss[0].IndexOf("checkBox1") >= 0)
                        {
                            checkBox1.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }
                        if (ss[0].IndexOf("checkBox2") >= 0)
                        {
                            checkBox2.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }
                        if (ss[0].IndexOf("checkBox3") >= 0)
                        {
                            checkBox3.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }
                        if (ss[0].IndexOf("checkBox4") >= 0)
                        {
                            checkBox4.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }
                        //if (ss[0].IndexOf("checkBox5") >= 0)
                        //{
                        //    checkBox5.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("checkBox6") >= 0)
                        //{
                        //    checkBox6.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("checkBox7") >= 0)
                        //{
                        //    checkBox7.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("checkBox8") >= 0)
                        //{
                        //    checkBox8.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("checkBox9") >= 0)
                        //{
                        //    checkBox9.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                        //    continue;
                        //}

                        if (ss[0].IndexOf("radioButton1") >= 0)
                        {
                            radioButton1.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }
                        if (ss[0].IndexOf("radioButton2") >= 0)
                        {
                            radioButton2.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }
                        if (ss[0].IndexOf("radioButton3") >= 0)
                        {
                            radioButton3.Checked = (ss[1].Replace("\r\n", "") == "TRUE") ? true : false;
                            continue;
                        }

                        if (ss[0].IndexOf("imagePictureBox1") >= 0)
                        {
                            imagePictureBox1 = ss[1].Replace("\r\n", "");
                            if (File.Exists(imagePictureBox1))
                            {
                                pictureBox1.Image = CreateImage(imagePictureBox1);
                            }

                            continue;
                        }
                        if (ss[0].IndexOf("imagePictureBox2") >= 0)
                        {
                            imagePictureBox2 = ss[1].Replace("\r\n", "");
                            if (File.Exists(imagePictureBox2))
                            {
                                pictureBox2.Image = CreateImage(imagePictureBox2);
                            }
                            continue;
                        }
                        if (ss[0].IndexOf("imagePictureBox3") >= 0)
                        {
                            imagePictureBox3 = ss[1].Replace("\r\n", "");
                            if (File.Exists(imagePictureBox3))
                            {
                                pictureBox3.Image = CreateImage(imagePictureBox3);
                            }
                            continue;
                        }
                        if (ss[0].IndexOf("imagePictureBox4") >= 0)
                        {
                            imagePictureBox4 = ss[1].Replace("\r\n", "");
                            if (File.Exists(imagePictureBox4))
                            {
                                pictureBox4.Image = CreateImage(imagePictureBox4);
                            }
                            continue;
                        }
                        if (ss[0].IndexOf("imagePictureBox5") >= 0)
                        {
                            imagePictureBox5 = ss[1].Replace("\r\n", "");
                            if (File.Exists(imagePictureBox5))
                            {
                                pictureBox5.Image = CreateImage(imagePictureBox5);
                            }
                            continue;
                        }
                        if (ss[0].IndexOf("imagePictureBox6") >= 0)
                        {
                            imagePictureBox6 = ss[1].Replace("\r\n", "");
                            if (File.Exists(imagePictureBox6))
                            {
                                pictureBox6.Image = CreateImage(imagePictureBox6);
                            }
                            continue;
                        }
                        //if (ss[0].IndexOf("imagePictureBox7") >= 0)
                        //{
                        //    imagePictureBox7 = ss[1].Replace("\r\n", "");
                        //    pictureBox7.Image = CreateImage(imagePictureBox7);
                        //    continue;
                        //}
                        //if (ss[0].IndexOf("imagePictureBox8") >= 0)
                        //{
                        //    imagePictureBox8 = ss[1].Replace("\r\n", "");
                        //    pictureBox8.Image = CreateImage(imagePictureBox7);
                        //    continue;
                        //}

                        if (ss[0].IndexOf("htmlPictureBox1") >= 0)
                        {
                            htmlPictureBox1 = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("htmlPictureBox2") >= 0)
                        {
                            htmlPictureBox2 = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("htmlPictureBox3") >= 0)
                        {
                            htmlPictureBox3 = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("htmlPictureBox4") >= 0)
                        {
                            htmlPictureBox4 = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("htmlPictureBox5") >= 0)
                        {
                            htmlPictureBox5 = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("htmlPictureBox6") >= 0)
                        {
                            htmlPictureBox6 = ss[1].Replace("\r\n", "");
                            continue;
                        }


                        if (ss[0].IndexOf("r_path") >= 0)
                        {
                            string path = ss[1].Replace("\r\n", "");
                            rexe = path + "\\x64\\Rscript.exe";

                            if (!File.Exists(rexe))
                            {
                                continue;
                            }
                            else
                            {
                                rpath_chg = true;
                            }
                            textBox1.Text = path;
                            continue;
                        }
                    }
                }
            }
            if (sr != null) sr.Close();
            if (!rpath_chg)
            {
                MessageBox.Show(rexe + " is not found.\nThe path of the loaded setting was ignored.");
                if (!File.Exists(rexe))
                {
                    MessageBox.Show("Please reconfigure the path where 'Rscript.exe' exists.");
                    return;
                }
            }
        }
        private void button15_Click(object sender, EventArgs e)
        {
            save();
        }

        private void button16_Click(object sender, EventArgs e)
        {
            load("");
        }
        private void UpdateInvokeRequire()
        {
            TimeSpan ts = stopwatch.Elapsed;
        }

        public string script_file_ = "";
        void Proc_Exited(object sender, EventArgs e)
        {
            System.Threading.Thread.Sleep(50);
            stopwatch.Stop();
            TimeSpan ts = stopwatch.Elapsed;
            if (InvokeRequired)
            {
                Invoke(new Action(this.UpdateInvokeRequire));
                //Invoke(new Action(() => { label1.Text = "Stop!"; }));
            }
        }

        public void execute_()
        {
            bool wait = true;
            ProcessStartInfo pInfo = new ProcessStartInfo();
            //pInfo.FileName = textBox1.Text + "\\R.exe";
            //pInfo.Arguments = "CMD BATCH  --vanilla " + script_file;

            //pInfo.FileName = textBox1.Text + "\\Rscript.exe";
            //pInfo.Arguments = "" + script_file;

            pInfo.FileName = textBox1.Text + "\\x64\\Rscript.exe";
            pInfo.Arguments = "" + script_file_;

            if (!File.Exists(pInfo.FileName))
            {
                MessageBox.Show(pInfo.FileName + " is not found.\nPlease confirm that " + textBox1.Text + " is specified as the file path, which is correct.");
                return;
            }
            //Process p = Process.Start(pInfo);
            Process p = new Process();
            p.StartInfo = pInfo;

            if (wait)
            {
                p.Start();
                p.WaitForExit();
            }
            else
            {
                stopwatch.Start();
                p.Exited += new EventHandler(Proc_Exited);
                p.EnableRaisingEvents = true;
                p.Start();
            }
        }
        public void execute(string script_file, bool wait = true)
        {
            ProcessStartInfo pInfo = new ProcessStartInfo();
            //pInfo.FileName = textBox1.Text + "\\R.exe";
            //pInfo.Arguments = "CMD BATCH  --vanilla " + script_file;

            //pInfo.FileName = textBox1.Text + "\\Rscript.exe";
            //pInfo.Arguments = "" + script_file;

            pInfo.FileName = textBox1.Text + "\\x64\\Rscript.exe";
            pInfo.Arguments = "" + script_file;

            if (!File.Exists(pInfo.FileName))
            {
                MessageBox.Show(pInfo.FileName + " is not found.\nPlease confirm that " + textBox1.Text + " is specified as the file path, which is correct.");
                return;
            }
            //Process p = Process.Start(pInfo);
            Process p = new Process();
            p.StartInfo = pInfo;

            if (wait)
            {
                p.Start();
                p.WaitForExit();
            }
            else
            {
                stopwatch.Start();
                p.Exited += new EventHandler(Proc_Exited);
                p.EnableRaisingEvents = true;
                p.Start();
            }
        }


        public void execute_bat(string script_file, bool wait = true)
        {
            ProcessStartInfo pInfo = new ProcessStartInfo();
            //pInfo.FileName = textBox1.Text + "\\R.exe";
            //pInfo.Arguments = "CMD BATCH  --vanilla " + script_file;

            //pInfo.FileName = textBox1.Text + "\\Rscript.exe";
            //pInfo.Arguments = "" + script_file;

            pInfo.FileName = script_file;
            pInfo.Arguments = "";

            if (!File.Exists(pInfo.FileName))
            {
                MessageBox.Show(pInfo.FileName + " is not found.\nPlease confirm that " + textBox1.Text + " is specified as the file path, which is correct.");
                return;
            }
            //Process p = Process.Start(pInfo);
            Process p = new Process();
            p.StartInfo = pInfo;

            if (wait)
            {
                p.Start();
                p.WaitForExit();
            }
            else
            {
                stopwatch.Start();
                p.Exited += new EventHandler(Proc_Exited);
                p.EnableRaisingEvents = true;
                p.Start();
            }
        }


        public ListBox GetNames()
        {
            if (File.Exists("names.txt"))
            {
                File.Delete("names.txt");
            }

            //string cmd1 = tft_header_ru();

            encoding = comboBox1.Text;
            string cmd = "";
            cmd += ".libPaths(c('" + RlibPath + "',.libPaths()))\r\n";
            cmd += "dir='" + work_dir.Replace("\\", "\\\\") + "'\r\n";
            cmd += "library(data.table)\r\n";
            cmd += "setwd(dir)\r\n";
            cmd += "#df <- fread(\"" + base_name + ".csv\", na.strings=c(\"\", \"NULL\"), header = TRUE, stringsAsFactors = TRUE)\r\n";
            cmd += "df <- read.csv(\"" + base_name + ".csv\", header=T, stringsAsFactors = F, na.strings = c(\"\", \"NA\"),fileEncoding=\"" + encoding + "\")\r\n";
            cmd += "x_<-ncol(df)\r\n";
            cmd += "print(x_)\r\n";
            cmd += "for ( i in 1:x_) print(names(df)[i])\r\n";

            string file = "tmp_get_namse.R";

            try
            {
                var encoding = new System.Text.UTF8Encoding(false);
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write("options(encoding=\"" + comboBox1.Text + "\")\r\n");

                    sw.Write("options(width=1000)\r\n");
                    sw.Write("sink(file = \"names.txt\")\r\n");
                    sw.Write(cmd);
                    sw.Write("sink()\r\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return null;
            }

            execute(file);
            ListBox list = new ListBox();


            if (File.Exists("names.txt"))
            {
                StreamReader sr = null;
                try
                {
                    sr = new StreamReader("names.txt", Encoding.GetEncoding(comboBox1.Text));
                    while (sr.EndOfStream == false)
                    {
                        string line = sr.ReadLine();
                        var nums = line.Split(' ');
                        int num = int.Parse(nums[1]);

                        for (int i = 0; i < num; i++)
                        {
                            line = sr.ReadLine();
                            var names = line.Substring(4);

                            names = names.Replace("\n", "");
                            names = names.Replace("\r", "");
                            names = names.Replace("\"", "");
                            if (names.IndexOf(" ") >= 0)
                            {
                                names = "'" + names + "'";
                            }
                            list.Items.Add(names);
                        }
                        if (list.Items.Count != num)
                        {
                            status = -1;
                            MessageBox.Show("Does the column name contain \", \" or \"spaces\"?\n" +
                                "ou may not be getting the column names correctly.", "警告", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        }
                        break;
                    }
                    sr.Close();
                }
                catch { sr.Close(); status = -1; }
            }
            else
            {
                status = -1;
            }

            return list;
        }


        public void listBox_remake()
        {
            ListBox colname_list = GetNames();

            listBox1.Items.Clear();
            listBox2.Items.Clear();
            listBox3.Items.Clear();

            comboBox2.Items.Clear();

            for (int i = 0; i < colname_list.Items.Count; i++)
            {
                listBox1.Items.Add(colname_list.Items[i]);
            }
            for (int i = 0; i < listBox1.Items.Count; i++)
            {
                comboBox2.Items.Add(listBox1.Items[i].ToString());
            }
            listBox2.Items.Add("mean");
            listBox2.Items.Add("sd");
            listBox2.Items.Add("var");
            listBox2.Items.Add("skewness");
            listBox2.Items.Add("kurtosis");
            listBox2.Items.Add("peak2peak");
            listBox2.Items.Add("RMS");
            listBox2.Items.Add("range");
            listBox2.Items.Add("CrestFactor");
            listBox2.Items.Add("ShapeFactor");
            listBox2.Items.Add("ImpulseFactor");
            listBox2.Items.Add("MarginFactor");
            listBox2.Items.Add("logEnergy");

            listBox2.Items.Add("spectrum");
            listBox2.Items.Add("spectral_mean");
            listBox2.Items.Add("spectral_std");
            listBox2.Items.Add("spectral_skewness");
            listBox2.Items.Add("spectral_kurtosis");
            listBox2.Items.Add("mahalanobis");

        }

        private void button1_Click(object sender, EventArgs e)
        {
            if (openFileDialog1.ShowDialog() != DialogResult.OK)
            {
                return;
            }
            System.IO.Directory.SetCurrentDirectory(exePath + "\\..\\..\\..\\..\\");
            Directory.CreateDirectory("work");
            System.IO.Directory.SetCurrentDirectory(exePath + "\\..\\..\\..\\..\\");

            Directory.CreateDirectory("Processed");
            Directory.CreateDirectory("Untreated");

            base_dir = System.IO.Directory.GetCurrentDirectory();
            work_dir = base_dir + "\\work";
            System.IO.Directory.SetCurrentDirectory(work_dir);


            csv_file = openFileDialog1.FileName;
            csv_dir = Path.GetDirectoryName(csv_file);
            base_name = Path.GetFileNameWithoutExtension(csv_file);



            string tmp = Path.GetDirectoryName(work_dir + "\\" + base_name + ".csv");
            if (csv_dir != Path.GetDirectoryName(work_dir + "\\" + base_name + ".csv"))
            {
                string bat = "..\\bin\\nkf.exe";
                bat += " -w " + csv_file + " > " + base_name + ".csv";

                var encoding2 = new System.Text.UTF8Encoding(false);

                string nkf_bat = base_name + "_nkf.bat";
                try
                {
                    using (System.IO.StreamWriter sw = new StreamWriter(nkf_bat, false, encoding2))
                    {
                        sw.Write(bat + "\n");
                    }
                }
                catch
                {
                    if (MessageBox.Show("nkf", "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                        return;
                }
                try
                {
                    execute_bat(nkf_bat);
                }
                catch
                {

                }
                //File.Copy(csv_file, base_name + ".csv", true);
            }
            base_name0 = base_name;


            this.Text = "[" + base_name + "]";
            listBox_remake();


            string file = exePath + "R_install_path.txt";

            var encoding = new System.Text.UTF8Encoding(false);

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(textBox1.Text + "\n");
                }
            }
            catch
            {
                if (MessageBox.Show("R_install_path", "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }
        }



        private void textBox3_TextChanged(object sender, EventArgs e)
        {

        }

        private void label14_Click(object sender, EventArgs e)
        {

        }

        private void tabPage1_Click(object sender, EventArgs e)
        {

        }

        private void button2_Click(object sender, EventArgs e)
        {

            string cmd = "";
            cmd += "base_name <<- '" + base_name + "'\r\n";
            cmd += "csv_encoding = '" + comboBox1.Text + "'\r\n";
            cmd += "#spline with smoothing\r\n";
            cmd += "use_spline = " + (checkBox1.Checked?"TRUE": "FALSE") +"\r\n";
            cmd += "\r\n";
            cmd += "#input data resolution\r\n";
            cmd += "unit_of_time = '" + comboBox4.Text+"'\r\n";
            cmd += "unit_of_record = "+textBox2.Text + "\r\n";
            cmd += "\r\n";
            cmd += "#Feature smoothing\r\n";
            cmd += "feature_smooth_window = " + textBox3.Text + "\r\n";
            cmd += "\r\n";
            cmd += "#Data length sent at once\r\n";
            cmd += "one_input = " + textBox4.Text +"\r\n";
            cmd += "#585936\r\n";
            cmd += "\r\n";
            cmd += "#input data smoothing(sampling)\r\n";
            cmd += "smooth_window = " + textBox5.Text + "\r\n";
            cmd += "smooth_window_slide = " + textBox6.Text + "\r\n";
            cmd += "#lowess smoothing\r\n";
            cmd += "use_lowess = " + (checkBox2.Checked ? "TRUE" : "FALSE") + "\r\n";
            cmd += "\r\n";
            cmd += "#Maximum data length used for prediction\r\n";
            cmd += "#max_data_len = 864000\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "#training period\r\n";
            cmd += "max_train_span = " + textBox7.Text + "\r\n";
            cmd += "#maximum retention length of incoming data\r\n";
            cmd += "max_retained_length = " + textBox8.Text + "\r\n";
            cmd += "\r\n";
            cmd += "###########################################################\r\n";
            cmd += "#       Below are the figures after moving average (after input data smoothing)\r\n";
            cmd += "###########################################################\r\n";
            cmd += "sampling_num <- " + textBox9.Text + "\r\n";
            cmd += "\r\n";
            cmd += "#The number of lookbacks when using feature values ​​(mean, variance, etc.) of each input variable as feature values\r\n";
            cmd += "lookback=" + textBox10.Text + "\r\n";
            cmd += "lookback_slide = " + textBox11.Text + "\r\n";
            cmd += "\r\n";
            cmd += "#Feature smoothing\r\n";
            cmd += "smooth_window2 = " + textBox13.Text + "\r\n";
            cmd += "smooth_window_slide2 = " + textBox12.Text + "\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "gyap_ratio = " + textBox19.Text + "\r\n";
            cmd += "smoother_span = " + textBox20.Text + "\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "#Score just before used for predictive model training\r\n";
            cmd += "train_num = " + textBox14.Text + "\r\n";
            cmd += "#monotonicity Score just before used for calculation\r\n";
            cmd += "monotonicity_num = " + textBox15.Text + "\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "#default threshold\r\n";
            cmd += "threshold = " + textBox16.Text + "\r\n";
            if (textBox21.Text == "")
            {
                cmd += "#threshold_target = 0\r\n";
            }else
            {
                cmd += "threshold_target = " + textBox21.Text + "\r\n";
            }
            cmd += "\r\n";
            cmd += "#plot用Ymax\r\n";
            cmd += "ymax = -10000\r\n";
            cmd += "ymin =  10000\r\n";
            cmd += "\r\n";
            cmd += "#Threshold for each feature value, Ymax parameter set\r\n";
            cmd += "feature_param = NULL\r\n";
            cmd += "\r\n";
            cmd += "#future length threshold to predict\r\n";
            cmd += "max_prediction_length = " + textBox17.Text + "\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "#Anomaly occurrence time measured from the predicted present\r\n";
            if (textBox18.Text != "")
            {
                cmd += "failure_time_init = " + textBox18.Text + "\r\n";
            } else
            {
                cmd += "failure_time_init = 1000*max_prediction_length*unit_of_record\r\n";
            }
            cmd += "failure_time = failure_time_init\r\n";
            cmd += "\r\n";
            cmd += "#Output time unit\r\n";
            cmd += "forecast_time_unit = '" + comboBox5.Text + "'\r\n";
            cmd += "\r\n";
            cmd += "#Anomaly model\r\n";
            cmd += "m_mahalanobis <- NULL\r\n";
            cmd += "\r\n";
            cmd += "#All incoming data, from the past to the present\r\n";
            cmd += "#A data frame limited to the maximum data length used for prediction\r\n";
            cmd += "pre = NULL\r\n";
            cmd += "pre_org = NULL\r\n";
            cmd += "#Data frame limited to maximum retention length\r\n";
            cmd += "past = NULL\r\n";
            cmd += "\r\n";
            cmd += "#Prediction model selection\r\n";
            cmd += "use_auto_arima = " + (radioButton1.Checked ? "TRUE" : "FALSE") + "\r\n";
            cmd += "use_arima = " + (radioButton2.Checked ? "TRUE" : "FALSE") + "\r\n";
            cmd += "use_ets = " + (radioButton3.Checked ? "TRUE" : "FALSE") + "\r\n";
            cmd += "use_plophet = " + (radioButton4.Checked ? "TRUE" : "FALSE") + "\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "#When data including anomalies can be input=TRUE\r\n";
            cmd += "abnormality_detected_data <- TRUE\r\n";
            cmd += "\r\n";
            cmd += "#Features to track\r\n";
            cmd += "tracking_feature <- NULL\r\n";
            cmd += "dynamic_threshold = TRUE\r\n";
            cmd += "watch_name = ''\r\n";
            cmd += "\r\n";
            cmd += "RUL <- c()\r\n";
            cmd += "pre = NULL\r\n";
            cmd += "past = NULL\r\n";
            cmd += "feature_param = NULL\r\n";
            cmd += "\r\n";
            cmd += "index_number <- 0\r\n";
            cmd += "time_Index <- 1\r\n";
            cmd += "\r\n";
            cmd += "timeStamp <- ''\r\n";
            cmd += "time_out <- 60*2\r\n";
            cmd += "save.image('./predictive_maintenance.RData')\r\n";

            string file = base_dir+"\\" +base_name0 + "_parameters.r";

            var encoding = new System.Text.UTF8Encoding(false);

            try
            {
                var utf8Encoding = new System.Text.UTF8Encoding(false);
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(cmd + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            if (work_dir == "") return;

            string cmd = "";
            System.IO.DirectoryInfo di = new System.IO.DirectoryInfo(work_dir + "\\Untreated");
            System.IO.FileInfo[] csvfiles =
                di.GetFiles("*.csv", System.IO.SearchOption.TopDirectoryOnly);
            foreach (System.IO.FileInfo f in csvfiles)
            {
                string name = Path.GetFileNameWithoutExtension(f.FullName);

                string tmp = work_dir + "\\Untreated\\" + name + ".csv";
                if (!File.Exists(tmp))
                {
                    File.Delete(tmp);
                }
            }

            di = new System.IO.DirectoryInfo(csv_dir);
            System.IO.FileInfo[] files =
                di.GetFiles("*.csv", System.IO.SearchOption.TopDirectoryOnly);

            cmd += "set data=" + csv_dir + "\r\n";
            cmd += "set serv=" + work_dir + "\\Untreated" + "\r\n";
            cmd += "del .\\work\\all.csv\r\n";
            cmd += "del /Q \"%serv%\\*.csv\"\r\n";

            foreach (System.IO.FileInfo f in files)
            {
                string name = Path.GetFileNameWithoutExtension(f.FullName);

                string tmp = work_dir + "\\Untreated\\" + name +".csv";
                if (!File.Exists(tmp))
                {
                    File.Copy(f.FullName, tmp, true);
                }
            }
            cmd += "for %%i in (\"%data%\\*.csv\")do ( \r\n";
            cmd += "  .\\bin\\nkf.exe -w \"%%i\" > \"%serv%\\%%~ni.csv\"\r\n";
            cmd += ")\r\n";

            cmd += ":copy /B \"%data%\\*.csv\" %serv% /v /y\r\n";

            string file = base_dir+"\\" + base_name0 + "_IoT_Emulator.bat";
            
            var encoding = new System.Text.UTF8Encoding(false);

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(cmd + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }

            cmd = "";
            cmd += ".libPaths(c('" + RlibPath + "',.libPaths()))\r\n";
            cmd += "library(data.table)\r\n";
            cmd += "source('./src/csv_division.r')\r\n";
            cmd += "file = \"";

            string csv_name = Path.GetFileNameWithoutExtension(csv_file);
            cmd += csv_name + ".csv";
            cmd += "\"\r\n";
            cmd += "size =" + textBox4.Text +"\r\n";
            cmd += "csv_division(file, size)\r\n";


            file = base_dir + "\\" + base_name0 + "_Emulator.r";

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(cmd + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }

            cmd = "";
            cmd += "call init.bat\r\n";
            cmd += ":call ..\\..\\setup_ini.bat\r\n";
            cmd += "\r\n";
            cmd += "set  R_LIBS_USER=.\\library\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "cd %~dp0\r\n";
            cmd += "\r\n";
            cmd += ".\\bin\\nkf.exe -w \"" + csv_file + "\" > " + csv_name + ".csv\r\n";
            cmd += "\r\n";
            cmd += "\"%R_INSTALL_PATH%\\bin\\x64\\Rscript.exe\" --vanilla "+ "\"" + file + "\"\r\n";


            file = base_dir + "\\" + base_name0 + "_Emulator.bat";

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(cmd + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }
            if (checkBox5.Checked)
            {
                System.IO.Directory.SetCurrentDirectory(base_dir);
                try
                {
                    execute_bat(file);
                }
                catch
                {

                }
            }
        }

        void monitoring_validation( bool validation = true)
        {
            if (listBox3.SelectedIndices.Count < 2)
            {
                if ( comboBox6.Text == "ja-JP")
                {
                    MessageBox.Show("追跡する特徴量を２個追加してください.");
                }else
                {
                    MessageBox.Show("Please add two features to track.");

                }
                status = -1;
                return;
            }
            if (textBox1.Text == "")
            {
                if (comboBox6.Text == "ja-JP")
                {
                    MessageBox.Show("Rの実行環境パスが未設定です");
                }
                else
                {
                    MessageBox.Show("R execution environment path is not set");

                }
                status = -1;
                return;
            }
            if (comboBox2.Text == "")
            {
                if (comboBox6.Text == "ja-JP")
                {
                    MessageBox.Show("データの時間項目が未選択です");
                }
                else
                {
                    MessageBox.Show("Time item of data is not selected");

                }
                status = -1;
                return;
            }

            string param_base = base_name0 + "_parameters.r";
            string param = work_dir + "\\parameters.r";

            File.Copy(base_dir+"\\" + param_base, param, true);

            string args = base_dir+"\\" + base_name0 + "_args.csv";


            ListBox arg = new ListBox();
            for ( int i = 0; i < listBox3.SelectedIndices.Count; i++)
            {
                var s = listBox3.Items[listBox3.SelectedIndices[i]].ToString().Split('.');
                bool dup = false;
                if ( arg.Items.Count >= 1)
                {
                    for ( int j = 0; j < arg.Items.Count; j++)
                    {
                        if (arg.Items[j].ToString() == s[0])
                        {
                            dup = true;
                            break;
                        }
                    }
                }
                if ( !dup ) arg.Items.Add(s[0]);
            }

            var encoding = new System.Text.UTF8Encoding(false);

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(args, false, encoding))
                {
                    sw.Write("var" + "\r\n");

                    sw.Write("\"" + arg.Items.Count.ToString() + "\"" + "\r\n");
                    for (int i = 0; i < arg.Items.Count; i++)
                    {
                        sw.Write("\""+arg.Items[i].ToString() + "\"" + "\r\n");
                    }
                    sw.Write("\"" + comboBox2.Text + "\"" + "\r\n");
                    sw.Write("\"" + comboBox3.Text + "\"" + "\r\n");
                    sw.Write("\"" + "mahalanobis" + "\"" + "\r\n");
                    for (int i = 0; i < listBox3.SelectedIndices.Count; i++)
                    {
                        var s = listBox3.Items[listBox3.SelectedIndices[i]].ToString();
                        sw.Write("\"" + s + "\"" + "\r\n");
                    }
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + args, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }


            string cmd = "";
            cmd += "call init.bat\r\n";
            cmd += ":call ..\\..\\setup_ini.bat\r\n";
            cmd += "\r\n";
            cmd += "set  R_LIBS_USER=.\\library\r\n";
            cmd += "\r\n";
            cmd += "set test=\"./src/predictive_maintenance2.r\"\r\n";
            cmd += "copy \"" + param_base + "\" work\\parameters.r /v /y\r\n";
            cmd += "copy \"" + base_name0 + "_args.csv" + "\" work\\args.csv /v /y\r\n";
            if (validation)
            {
                cmd += "del \"./work\\" + base_name0 + "_feature_param.csv\"\r\n";
            }
            cmd += "\r\n";
            cmd += "cd %~dp0\r\n";
            cmd += "\r\n";
            cmd += "del /Q images\\*.png\r\n";
            cmd += "del /Q images\\*.r\r\n";
            cmd += "del /Q images\\debug\\*.png\r\n";
            cmd += "\r\n";
            cmd += "\"%R_INSTALL_PATH%\\bin\\x64\\Rscript.exe\" --vanilla %test% "
                    + " " + "args.csv"  + "\r\n";

            //mahalanobis vibration.kurtosis vibration.mean datetime + \r\n";

            string file = base_dir+"\\" + base_name0 + "_test.bat";
            if (!validation)
            {
                file = base_dir+"\\" + base_name0 + "_execute.bat";
            }
            
            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, encoding))
                {
                    sw.Write(cmd + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }

            if (!validation)
            {
                if (File.Exists(work_dir + "\\feature_param.csv"))
                {
                    string bak_file = "";
                    for (int i = 1; i < 1000; i++)
                    {
                        bak_file = string.Format(work_dir + "\\feature_param_{0}bak({1}).csv", base_name0, exist_number);
                        if (File.Exists(bak_file))
                        {
                            exist_number++;
                        }
                        else
                        {
                            break;
                        }
                    }
                    File.Copy(work_dir + "\\feature_param.csv", bak_file, true);
                    File.Delete(work_dir + "\\feature_param.csv");
                }
            }

            if ( checkBox5.Checked)
            {
                System.IO.Directory.SetCurrentDirectory(base_dir);
                button2_Click(null, null);
                button3_Click(null, null);

                try
                {
                    execute_bat(file, false);
                }
                catch
                {

                }
            }
        }
        private void button4_Click(object sender, EventArgs e)
        {
            monitoring_validation(true);
        }

        private void button5_Click(object sender, EventArgs e)
        {
            monitoring_validation(false);
        }

        private void button6_Click(object sender, EventArgs e)
        {
            if (timer1.Enabled)
            {
                button6.Text = "!Monitor!";
                timer1.Enabled = false;
                timer1.Stop();
            }
            else
            {
                button6.Text = "Monitor stop";
                timer1.Enabled = true;
                timer1.Start();
            }
        }
        public void GetImages_()
        {
            imageFiles = Directory
              .GetFiles(work_dir + "\\..\\images", "*.png", SearchOption.TopDirectoryOnly)
              .Where(filePath => Path.GetFileName(filePath) != ".DS_Store")
              .OrderBy(filePath => File.GetLastWriteTime(filePath).Date)
              .ThenBy(filePath => File.GetLastWriteTime(filePath).TimeOfDay)
              .ToList();
        }
        public void GetImages()
        {
            GetImages_();
            if (imageFiles.Count == 0)
            {
                return;
            }

            string fileName = imageFiles[0];

            if (System.IO.File.Exists(fileName))
            {
                pictureBox1.Image = CreateImage(fileName);
                imagePictureBox1 = fileName;
            }
            else
            {
                MessageBox.Show("1st image is missing");
                return;
            }


            max_image = imageFiles.Count;

            if (max_image >= max_image_limit - 1)
            {
                MessageBox.Show("Exceeded maximum number of sequence images");
            }
            trackBar1.Maximum = max_image - 1;
            trackBar1.Minimum = 0;

            numericUpDown1.Maximum = max_image - 1;
            numericUpDown1.Minimum = 0;
        }
        private void timer1_Tick(object sender, EventArgs e)
        {
            if (work_dir == "") return;

            timer1.Enabled = false;
            GetImages();
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            timer1.Enabled = true;
            animation_stop = true;

            try
            {

                var sv_num = max_image;
                GetImages();
                string fileName = imageFiles[max_image - 1];

                if (System.IO.File.Exists(fileName))
                {
                    pictureBox1.Image = CreateImage(fileName);
                    imagePictureBox1 = fileName;
                }

                if (sv_num == max_image)
                {
                    return;
                }
                button6_Click(sender, e);
            }
            catch { }
        }
        private void view()
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            var fileName = imageFiles[num_image];
            if (System.IO.File.Exists(fileName))
            {
                pictureBox1.Image = CreateImage(fileName);
                pictureBox1.Refresh();
                imagePictureBox1 = fileName;
            }
        }

        private void trackBar1_Scroll(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            num_image = trackBar1.Value;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button7_Click(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            num_image = 0;
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button8_Click(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            num_image = max_image - 1;
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button10_Click(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            num_image--;
            if (num_image < 0)
            {
                num_image = 0;
            }
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button9_Click(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            num_image++;
            if (num_image >= max_image)
            {
                num_image = max_image - 1;
            }
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }
        private void animation()
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            for (int i = num_image; i < max_image; i++)
            {
                num_image = i;
                trackBar1.Value = num_image;
                numericUpDown1.Value = num_image;
                view();
                if (animation_stop) break;
                System.Threading.Thread.Sleep(50);
            }
            animation_stop = false;
        }
        private void button12_Click(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            animation_stop = false;

            Task task = Task.Run(() => {
                animation();
            });
        }

        private void button11_Click(object sender, EventArgs e)
        {
            animation_stop = true;
        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            checkBox3.Checked = checkBox2.Checked;
        }

        private void checkBox3_CheckedChanged(object sender, EventArgs e)
        {
            checkBox2.Checked = checkBox3.Checked;
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            checkBox4.Checked = checkBox1.Checked;
        }

        private void checkBox4_CheckedChanged(object sender, EventArgs e)
        {
            checkBox1.Checked = checkBox4.Checked;
        }

        private void button13_Click(object sender, EventArgs e)
        {
            if ( listBox2.Text != "" && listBox1.Text != "")
            {
                listBox3.Items.Add(listBox1.Text + "."+listBox2.Text);
                listBox3.SetSelected(listBox3.Items.Count - 1, true);
            }
        }

        private void pictureBox1_Click(object sender, EventArgs e)
        {
            if (imageFiles == null || imageFiles.Count == 0)
            {
                return;
            }
            var fileName = imageFiles[num_image];
            if (System.IO.File.Exists(fileName))
            {
                Form2 f = new Form2();
                f.form1_ = this;
                f.SetFile(work_dir, fileName);
                f.button9.Visible = true;
                f.button10.Visible = true;
                f.timer1.Enabled = true;
                f.timer1.Start();

                f.Show();
            }
        }

        public void summary(object sender, EventArgs e, bool summary = true)
        {
            string cmd = "";
            cmd += "options(encoding = '" + comboBox1.Text +"')\r\n";
            cmd += "\r\n";
            cmd += "curdir = getwd()\r\n";
            cmd += ".libPaths(c('" + RlibPath + "',.libPaths()))\r\n";
            cmd += "dir='" + work_dir.Replace("\\", "\\\\") + "'\r\n";
            cmd += "library(data.table)\r\n";
            cmd += "setwd(dir)\r\n";
            cmd += "library(plotly)\r\n";

            cmd += "\r\n";
            cmd += "putpng_path= paste(curdir, '/images/', sep='')\r\n";
            cmd += "source('../src/predictive_maintenance_funcs.r')\r\n";
            cmd += "source('parameters.r')\r\n";
            cmd += "source('../src/feature_summary_visualization.r')\r\n";
            cmd += "\r\n";
            cmd += "sigin_arg = '" + comboBox3.Text + "'\r\n";
            cmd += "#tracking_feature_= ''\r\n";
            cmd += "\r\n";
            cmd += "initial_pm(sigin_arg)\r\n";
            cmd += "abnormality_detected_data <- FALSE\r\n";
            cmd += "#watch_name <<- paste(tracking_feature_, '..', sep='')\r\n";
            cmd += "\r\n";
            cmd += "sigin <<- 1.0\r\n";
            cmd += "if ( sigin_arg == '-' )\r\n";
            cmd += "{\r\n";
            cmd += "	sigin <<- -1.0\r\n";
            cmd += "}\r\n";
            cmd += "\r\n";
            cmd += "smooth_window <<- " + textBox5.Text + "\r\n";
            cmd += "smooth_window_slide <<- " + textBox6.Text + "\r\n";
            cmd += "smooth_window2 <<- " + textBox13.Text + "\r\n";
            cmd += "smooth_window_slide2 <<- " + textBox12.Text + "\r\n";
            cmd += "\r\n";
            cmd += "lookback <<- " + textBox10.Text + "\r\n";
            cmd += "lookback_slide <<- " + textBox11.Text + "\r\n";
            cmd += "#lookback <<- 24\r\n";
            cmd += "#lookback_slide <<- 24\r\n";
            cmd += "#平滑化をlowessで行う\r\n";
            cmd += "use_lowess = " + ((checkBox3.Checked) ? "TRUE" : "FALSE") + "\r\n";
            cmd += "smoother_span <<- 0.05\r\n";
            cmd += "\r\n";
            cmd += "\r\n";
            cmd += "timeStamp <- '" + comboBox2.Text + "'\r\n";
            cmd += "csvfile = '" + base_name + ".csv'\r\n";
            cmd += "base_name <<- '" + base_name + "'\r\n";
            cmd += "plt <- feature_summary_visualization(csvfile, timeStamp, summary=";
            if ( summary ) cmd += "T)\r\n";
            else cmd += "F)\r\n";
            cmd += "\r\n";
            cmd += "\r\n";

            string file = base_dir +"\\" + base_name0 + "_feat_visualize.r";
            if (!summary)
            {
                file = base_dir +"\\" + base_name0 + "_feature_discovery.r";
            }
            try
            {
                var utf8Encoding = new System.Text.UTF8Encoding(false);
                using (System.IO.StreamWriter sw = new StreamWriter(file, false, utf8Encoding))
                {
                    sw.Write(cmd + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + file, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }

            string param_base = base_name0 + "_parameters.r";
            string param = work_dir + "\\parameters.r";

            try
            {
                File.Copy(base_dir +"\\" + param_base, param, true);
                File.Copy(csv_dir + "\\" + base_name + ".csv", base_name + ".csv", true);
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot copy in " + param_base, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }

            string feat_visualize_base = base_name0 + "_feat_visualize.r";
            if ( !summary)
            {
                feat_visualize_base = base_name0 + "_feature_discovery.r";
            }
            string bat = "";
            bat += "call init.bat\r\n";
            bat += ":call ..\\..\\setup_ini.bat\r\n";
            bat += "\r\n";
            bat += "set  R_LIBS_USER=.\\library\r\n";
            bat += "\r\n";
            bat += "copy \"" + param_base + "\" work\\parameters.r /v /y\r\n";
            bat += "\r\n";
            bat += "cd %~dp0\r\n";
            bat += "set curdir=%~dp0\r\n";
            bat += "cd %curdir%\r\n";
            bat += "\r\n";
            bat += "del /Q images\\*.png\r\n";
            bat += "del /Q images\\*.r\r\n";
            bat += "del /Q images\\debug\\*.png\r\n";
            bat += "\r\n";
            bat += "\"%R_INSTALL_PATH%\\bin\\x64\\Rscript.exe\" --vanilla \"" + feat_visualize_base + "\"\r\n";

            if (!summary)
            {
                bat += "\r\n";
                bat += "cd bin\r\n";
                bat += "del feature_discovery_output.txt\r\n";
                bat += "sorting_el.exe " + "\"" + "%curdir%images\"\r\n";
                bat += "sorting_selection_el.exe " + "\"" + "%curdir%images\"\r\n";
                bat += "\r\n";
                bat += "copy feature_discovery_output.txt " + "\"" + csv_dir + "\" /v /y\r\n";
                bat += "cd %curdir%\r\n";
            }

            string batfile = base_dir+"\\" + base_name0 + "_feature_summary_visualization.bat";
            if (!summary)
            {
                batfile = base_dir+"\\" + base_name0 + "_feature_discovery.bat";
            }

            var encoding = new System.Text.UTF8Encoding(false);

            try
            {
                using (System.IO.StreamWriter sw = new StreamWriter(batfile, false, encoding))
                {
                    sw.Write(bat + "\n");
                }
            }
            catch
            {
                status = -1;
                if (MessageBox.Show("Cannot write in " + batfile, "", MessageBoxButtons.OK, MessageBoxIcon.Error) == DialogResult.OK)
                    return;
            }
            System.IO.Directory.SetCurrentDirectory(base_dir);

            if (checkBox5.Checked)
            {
                button2_Click(null, null);
                button3_Click(null, null);
                try
                {
                    execute_bat(batfile);
                }
                catch
                {

                }
                System.IO.Directory.SetCurrentDirectory(work_dir);

                if (summary)
                {
                    button21_Click(sender, e);
                }
            }
        }


        private void button14_Click(object sender, EventArgs e)
        {
            summary(sender, e, true);
        }

        private void pictureBox2_Click(object sender, EventArgs e)
        {
            if (System.IO.File.Exists(imagePictureBox2))
            {
                Form2 f = new Form2();
                f.form1_ = this;
                if (htmlPictureBox2 != "")
                {
                    f.plotly_html = htmlPictureBox2;
                }
                f.SetFile(work_dir, imagePictureBox2, false);

                f.Show();
            }
        }

        private void pictureBox3_Click(object sender, EventArgs e)
        {
            if (System.IO.File.Exists(imagePictureBox3))
            {
                Form2 f = new Form2();
                f.form1_ = this;
                if (htmlPictureBox3 != "")
                {
                    f.plotly_html = htmlPictureBox3;
                }
                f.SetFile(work_dir, imagePictureBox3, false);

                f.Show();
            }
        }

        private void pictureBox4_Click(object sender, EventArgs e)
        {
            if (System.IO.File.Exists(imagePictureBox4))
            {
                Form2 f = new Form2();
                f.form1_ = this;
                if (htmlPictureBox4 != "")
                {
                    f.plotly_html = htmlPictureBox4;
                }
                f.SetFile(work_dir, imagePictureBox4, false);

                f.Show();
            }
        }

        private void pictureBox5_Click(object sender, EventArgs e)
        {
            if (System.IO.File.Exists(imagePictureBox5))
            {
                Form2 f = new Form2();
                f.form1_ = this;
                if (htmlPictureBox5 != "")
                {
                    f.plotly_html = htmlPictureBox5;
                }
                f.SetFile(work_dir, imagePictureBox5, false);

                f.Show();
            }
        }

        private void pictureBox6_Click(object sender, EventArgs e)
        {
            if (System.IO.File.Exists(imagePictureBox6))
            {
                Form2 f = new Form2();
                f.form1_ = this;
                if ( htmlPictureBox6 != "")
                {
                    f.plotly_html = htmlPictureBox6;
                }
                f.SetFile(work_dir, imagePictureBox6, false);

                f.Show();
            }
        }

        private void button17_Click(object sender, EventArgs e)
        {
            listBox3.Items.Clear();
        }

        private void button18_Click(object sender, EventArgs e)
        {
            summary(sender, e, false);
        }

        private void button19_Click(object sender, EventArgs e)
        {
            bool x = checkBox5.Checked;

            checkBox5.Checked = false;
            //save();
            button2_Click(sender, e);
            button3_Click(sender, e);
            button4_Click(sender, e);
            button5_Click(sender, e);
            button14_Click(sender, e);
            button18_Click(sender, e);
            checkBox5.Checked = x;
        }

        private void comboBox6_TextChanged(object sender, EventArgs e)
        {

                
            string Language = comboBox6.Text;

            using (StreamWriter sw = new StreamWriter(exePath + "Language.txt"))
            {
                sw.Write(comboBox6.Text);
            }
            if (Language == "ja-JP")
            {
                if (!LanguageChangeMessageOff) MessageBox.Show("設定は次回の起動から有効になります");
                System.Threading.Thread.CurrentThread.CurrentUICulture = new System.Globalization.CultureInfo(comboBox6.Text);
            }
            if (Language == "en-US")
            {
                if (!LanguageChangeMessageOff)MessageBox.Show("Settings will take effect from the next startup");
                System.Threading.Thread.CurrentThread.CurrentUICulture = new System.Globalization.CultureInfo(comboBox6.Text);
            }
        }

        private void button20_Click(object sender, EventArgs e)
        {
            load_feature_discovery_output("");
        }
        private void load_feature_discovery_output(string setting_file)
        {
            //
            string file = csv_dir+"\\feature_discovery_output.txt";

            if (setting_file == "")
            {
                if (base_name0 == "")
                {
                    status = -1;
                    MessageBox.Show("input csv file !");
                    return;
                }
                if (!File.Exists(file)) save();

                if (!File.Exists(file))
                {
                    MessageBox.Show("file not found[" + file +"]");
                }
                listBox3.Items.Clear();
            }
            else
            {
                file = setting_file;
            }


            System.IO.StreamReader sr = new System.IO.StreamReader(file, Encoding.GetEncoding("SHIFT_JIS"));
            if (sr != null)
            {
                while (sr.EndOfStream == false)
                {
                    string s = sr.ReadLine();

                    listBox3.Items.Add(s.Replace("\n", ""));
                    listBox3.SetSelected(0, true);
                    
                    s = sr.ReadLine();

                    listBox3.Items.Add(s.Replace("\n", ""));
                    listBox3.SetSelected(1, true);


                    while (sr.EndOfStream == false)
                    {
                        s = sr.ReadLine();
                        var ss = s.Split(',');


                        if (ss[0].IndexOf("textBox5") >= 0)
                        {
                            textBox5.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox6") >= 0)
                        {
                            textBox6.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox10") >= 0)
                        {
                            textBox10.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox11") >= 0)
                        {
                            textBox11.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox12") >= 0)
                        {
                            textBox12.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox13") >= 0)
                        {
                            textBox13.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("textBox21") >= 0)
                        {
                            textBox21.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                        if (ss[0].IndexOf("comboBox3") >= 0)
                        {
                            comboBox3.Text = ss[1].Replace("\r\n", "");
                            continue;
                        }
                    }
                }
            }
            if (sr != null) sr.Close();
        }

        private void button21_Click(object sender, EventArgs e)
        {
            System.IO.Directory.SetCurrentDirectory(base_dir);

            System.IO.Directory.SetCurrentDirectory(work_dir);

            if (File.Exists(base_name0 + "_feature_df.png"))
            {
                pictureBox2.Image = CreateImage(base_name0 + "_feature_df.png");
                imagePictureBox2 = base_name0 + "_feature_df.png";
                htmlPictureBox2 = base_name0 + "_feature_df.html";
            }
            if (File.Exists(base_name0 + "_monotonicity2.png"))
            {
                pictureBox3.Image = CreateImage(base_name0 + "_monotonicity2.png");
                imagePictureBox3 = base_name0 + "_monotonicity2.png";
                htmlPictureBox3 = base_name0 + "_monotonicity2.html";
            }
            if (File.Exists(base_name0 + "_tracking_feature.png"))
            {
                pictureBox4.Image = CreateImage(base_name0 + "_tracking_feature.png");
                imagePictureBox4 = base_name0 + "_tracking_feature.png";
                htmlPictureBox4 = base_name0 + "_feature_summary_visualization1.html";
            }
            if (File.Exists(base_name0 + "_tracking_feature2.png"))
            {
                pictureBox5.Image = CreateImage(base_name0 + "_tracking_feature2.png");
                imagePictureBox5 = base_name0 + "_tracking_feature2.png";
                htmlPictureBox5 = base_name0 + "_feature_summary_visualization2.html";
            }
            if (File.Exists(base_name0 + "_input.png"))
            {
                pictureBox6.Image = CreateImage(base_name0 + "_input.png");
                imagePictureBox6 = base_name0 + "_input.png";
                htmlPictureBox6 = base_name0 + "_input.html";
            }
        }
    }
}
