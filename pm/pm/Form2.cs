using Microsoft.Web.WebView2.Core;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Media.Imaging;

namespace pm
{
    public partial class Form2 : Form
    {
        public Form2()
        {
            InitializeComponent();
            InitializeAsync();
            webView21.NavigationCompleted += webView21_NavigationCompleted;
        }
        async void InitializeAsync()
        {
            try
            {
                await webView21.EnsureCoreWebView2Async(null);
            }
            catch (Exception)
            {
                MessageBox.Show("The WebView2 runtime may not be installed.", Application.ProductName, MessageBoxButtons.OK, MessageBoxIcon.Warning);
                this.Close();
            }
        }

        private void webView21_NavigationCompleted(object sender, CoreWebView2NavigationCompletedEventArgs e)
        {
            if (webView21.CoreWebView2 != null)
            {
                //Web画面からVB/C＃へのホストオブジェクトにアクセスする必要がなければ
                webView21.CoreWebView2.Settings.AreHostObjectsAllowed = false;

                //Webコンテンツ(JavaScript)からVB／C＃側へのメッセージを処理する必要がなければ
                //webView21.CoreWebView2.Settings.IsWebMessageEnabled = false;

                //Web画面でJavaScriptを使用したくなければ
                //webView21.CoreWebView2.Settings.IsScriptEnabled = false;

                //alertやpromptなどのダイアログを表示したくなければ
                webView21.CoreWebView2.Settings.AreDefaultScriptDialogsEnabled = false;
            }
        }

        bool loadHtml(string html)
        {
            if (System.IO.File.Exists(html))
            {
                string webpath = (work_dir+"\\"+html).Replace("\\", "/").Replace("//", "/");
                webpath = "file:///" + webpath;
                try
                {
                    webView21.Source = new Uri(webpath);
                    if (webView21.CoreWebView2 != null)
                    {
                        //webView21.CoreWebView2.Navigate(webpath);
                    }
                    //webView21.Reload();
                    webView21.Update();
                    webView21.Refresh();
                }
                catch
                {
                    return false;
                }
            }
            else
            {
                return false;
            }

            return true;
        }

        string image_file = "";
        string work_dir = "";
        public Form1 form1_ = null;

        public string plotly_html = "";

        public void SetFile( string dir, string file,  bool checkBox1Checked = true)
        {
            image_file = file;
            work_dir = dir;
            pictureBox1.Image = CreateImage(image_file);
            pictureBox1.Show();
            if ( plotly_html != "")
            {
                pictureBox1.Visible = false;
                webView21.Visible = true;
                loadHtml(plotly_html);
                webView21.Show();
                checkBox1.Checked = checkBox1Checked;
                if (!checkBox1Checked)
                {
                    webView21.Visible = false;
                    pictureBox1.Visible = true;
                }
            }
        }

        public System.Drawing.Image CreateImage(string filename)
        {
            System.Drawing.Image img = null;
            try
            {
                System.IO.FileStream fs = new System.IO.FileStream(
                    filename,
                    System.IO.FileMode.Open,
                    System.IO.FileAccess.Read);
                img = System.Drawing.Image.FromStream(fs);
                fs.Close();
            }
            catch
            {
                img = null;
            }
            pictureBox1.Show();

            return img;
        }
        private void pictureBox1_Click(object sender, EventArgs e)
        {
            if (pictureBox1.SizeMode == PictureBoxSizeMode.Zoom)
            {
                panel2.AutoScroll = true;
                pictureBox1.SizeMode = PictureBoxSizeMode.AutoSize;
                pictureBox1.Dock = DockStyle.None;
                pictureBox1.Refresh();
                return;
            }
            if (pictureBox1.SizeMode == PictureBoxSizeMode.AutoSize)
            {
                panel2.AutoScroll = true;
                pictureBox1.SizeMode = PictureBoxSizeMode.Zoom;
                pictureBox1.Image = CreateImage(image_file);
                pictureBox1.Dock = DockStyle.Fill;
                pictureBox1.Refresh();
                return;
            }
        }

        private void pictureBox1_Resize(object sender, EventArgs e)
        {
            //if (pictureBox1.SizeMode == PictureBoxSizeMode.Zoom)
            //{
            //    pictureBox1.Size = new System.Drawing.Size(new Point(panel2.Width, panel2.Height));
            //}
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            timer1.Enabled = form1_.timer1.Enabled;
            if (!timer1.Enabled)
            {
                timer1.Stop();
                return;
            }

            form1_.GetImages_();
            string fileName = form1_.imageFiles[form1_.max_image - 1];

            if (System.IO.File.Exists(fileName))
            {
                if (image_file != fileName)
                {
                    image_file = fileName;
                    pictureBox1.Image = CreateImage(fileName);
                    pictureBox1.Show();
                }
            }
        }

        private void button8_Click(object sender, EventArgs e)
        {
            try
            {
                Bitmap bmp = new Bitmap(pictureBox1.Image);
                Clipboard.SetImage(bmp);
                bmp.Dispose();
            }
            catch
            {
            }
        }
        public static void CreateAnimatedGif(string savePath, string[] imageFiles)
        {
            GifBitmapEncoder encoder = new GifBitmapEncoder();

            foreach (string f in imageFiles)
            {
                BitmapFrame bmpFrame =
                    BitmapFrame.Create(new Uri(f, UriKind.RelativeOrAbsolute));
                encoder.Frames.Add(bmpFrame);
            }

            FileStream outputFileStrm = new FileStream(savePath,
                FileMode.Create, FileAccess.Write, FileShare.None);
            encoder.Save(outputFileStrm);
            outputFileStrm.Close();
        }

        private void button9_Click(object sender, EventArgs e)
        {

            string[] imageFiles_tmp = new string[form1_.imageFiles.Count];
            for (int i = 0; i < form1_.imageFiles.Count; i++)
            {
                imageFiles_tmp[i] = form1_.imageFiles[i];
            }
            CreateAnimatedGif(work_dir + "\\result.gif", imageFiles_tmp);
        }

        private void button10_Click(object sender, EventArgs e)
        {
            Directory.CreateDirectory("tmp");

            for (int i = 0; i < form1_.imageFiles.Count; i++)
            {
                string dest = String.Format(work_dir+"\\tmp\\result-{0:000000}.png", i + 1);
                File.Copy(form1_.imageFiles[i], dest, true);
            }
            Assembly myAssembly = Assembly.GetEntryAssembly();
            string path = System.IO.Path.GetDirectoryName(myAssembly.Location);

            string ffmpeg = path + "\\ffmpeg.exe";
            var app = new ProcessStartInfo();
            app.FileName = ffmpeg;
            app.Arguments = " -i " + work_dir + "\\tmp\\result-%06d.png image.avi";

            var p = Process.Start(app);
            p.WaitForExit();

            var imageFiles_tmp = Directory
              .GetFiles(work_dir + "\\tmp", "*.png", SearchOption.TopDirectoryOnly)
              .Where(filePath => Path.GetFileName(filePath) != ".DS_Store")
              .OrderBy(filePath => File.GetLastWriteTime(filePath).Date)
              .ThenBy(filePath => File.GetLastWriteTime(filePath).TimeOfDay)
              .ToList();

            for (int i = 0; i < imageFiles_tmp.Count; i++)
            {
                File.Delete(imageFiles_tmp[i]);
            }
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            if (checkBox1.Checked && plotly_html != "")
            {
                pictureBox1.Visible = false;
                webView21.Visible = true;
                webView21.Show();
            }else
            {
                pictureBox1.Visible = true;
                webView21.Visible = false;
                pictureBox1.Show();
            }
        }
    }
}
