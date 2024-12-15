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

namespace Sequential_number_viewer
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
            pictureBox1.AllowDrop = true;
        }

        public List<string> imageFiles = null;
        public int start_index = 1;
        public int max_image_limit = 100000;

        public int num_image = 0;
        public int step_image = 1;
        public int max_image = 0;
        public bool animation_stop = false;

        public static System.Drawing.Image CreateImage(string filename)
        {
            System.IO.FileStream fs = new System.IO.FileStream(
                filename,
                System.IO.FileMode.Open,
                System.IO.FileAccess.Read);
            System.Drawing.Image img = System.Drawing.Image.FromStream(fs);
            fs.Close();
            return img;
        }

        private void pictureBox1_Click(object sender, EventArgs e)
        {

        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {
            System.IO.Directory.SetCurrentDirectory(textBox1.Text);


            imageFiles = Directory
              .GetFiles(textBox1.Text, "*.png", SearchOption.TopDirectoryOnly)
              .Where(filePath => Path.GetFileName(filePath) != ".DS_Store")
              .OrderBy(filePath => File.GetLastWriteTime(filePath).Date)
              .ThenBy(filePath => File.GetLastWriteTime(filePath).TimeOfDay)
              .ToList();


            string fileName = imageFiles[0];

            if (System.IO.File.Exists(fileName))
            {
                pictureBox1.Image = CreateImage(fileName);
            }
            else
            {
                MessageBox.Show("1st image is missing");
                return;
            }


            max_image = imageFiles.Count;

            if ( max_image >= max_image_limit-1)
            {
                MessageBox.Show("Exceeded maximum number of sequence images");
            }
            trackBar1.Maximum = max_image-1;
            trackBar1.Minimum = 0;

            numericUpDown1.Maximum = max_image - 1;
            numericUpDown1.Minimum = 0;
        }
        private void button1_Click(object sender, EventArgs e)
        {
            num_image = 0;
            openFileDialog1.InitialDirectory = Environment.SpecialFolder.Desktop.ToString();
            if ( openFileDialog1.ShowDialog() != DialogResult.OK )
            {
                return;
            }

            textBox1.Text = System.IO.Path.GetDirectoryName(openFileDialog1.FileName);
            textBox1_TextChanged(sender, e);
        }

        private void view()
        {
            var fileName = imageFiles[num_image];
            if (System.IO.File.Exists(fileName))
            {
                pictureBox1.Image = CreateImage(fileName);
                pictureBox1.Refresh();
            }
        }
        private void button2_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            num_image--;
            if (num_image < 0)
            {
                num_image = 0;
            }
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button3_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            num_image++;
            if (num_image >= max_image)
            {
                num_image = max_image-1;
            }
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void trackBar1_Scroll(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            num_image = trackBar1.Value;
            numericUpDown1.Value = num_image;
            view();
        }

        private void animation()
        {
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
        private void button4_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            animation_stop = false;

            Task task = Task.Run(() => {
                animation();
            });
        }

        private void button5_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            num_image = 0;
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button6_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            num_image = max_image - 1;
            trackBar1.Value = num_image;
            numericUpDown1.Value = num_image;
            view();
        }

        private void button7_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
            animation_stop = true;
        }

        private void button8_Click(object sender, EventArgs e)
        {
            if (textBox1.Text == "") return;
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
            if (textBox1.Text == "") return;

            string[] imageFiles_tmp = new string[imageFiles.Count];
            for ( int i = 0; i < imageFiles.Count; i++)
            {
                imageFiles_tmp[i] = imageFiles[i];
            }
            CreateAnimatedGif(textBox1.Text+ "\\result.gif", imageFiles_tmp);
        }

        private void panel1_DragDrop(object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(DataFormats.FileDrop)) return;

            string[] dragFilePathArr = (string[])e.Data.GetData(DataFormats.FileDrop, false);
            textBox1.Text = System.IO.Path.GetDirectoryName(dragFilePathArr[0]);
            textBox1_TextChanged(sender, e);
        }

        private void panel1_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.All;
        }

        private void numericUpDown1_ValueChanged(object sender, EventArgs e)
        {
            num_image = (int)(numericUpDown1.Value);
            trackBar1.Value = num_image;
            view();
        }

        private void button10_Click(object sender, EventArgs e)
        {
            Directory.CreateDirectory("tmp");

            for ( int i = 0; i < imageFiles.Count; i++)
            {
                string dest = String.Format("tmp\\result-{0:000000}.png", i+1);
                File.Copy(imageFiles[i],dest, true);
            }
            Assembly myAssembly = Assembly.GetEntryAssembly();
            string path = System.IO.Path.GetDirectoryName(myAssembly.Location);

            string ffmpeg = path + "\\ffmpeg.exe";
            var app = new ProcessStartInfo();
            app.FileName = ffmpeg;
            app.Arguments = "-framerate 1 -i " + textBox1.Text+ "\\tmp\\result-%06d.png" +
                " -r 2"+
                " image.avi";

            var p = Process.Start(app);
            p.WaitForExit();

            var imageFiles_tmp = Directory
              .GetFiles(textBox1.Text + "\\tmp", "*.png", SearchOption.TopDirectoryOnly)
              .Where(filePath => Path.GetFileName(filePath) != ".DS_Store")
              .OrderBy(filePath => File.GetLastWriteTime(filePath).Date)
              .ThenBy(filePath => File.GetLastWriteTime(filePath).TimeOfDay)
              .ToList();

            for (int i = 0; i < imageFiles_tmp.Count; i++)
            {
                File.Delete(imageFiles_tmp[i]);
            }

        }

        private void button11_Click(object sender, EventArgs e)
        {
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            animation_stop = true;

            try
            {

                var sv_num = max_image;
                textBox1_TextChanged(sender, e);
                string fileName = imageFiles[max_image - 1];

                if (System.IO.File.Exists(fileName))
                {
                    pictureBox1.Image = CreateImage(fileName);
                }

                if (sv_num == max_image)
                {
                    return;
                }
                button6_Click(sender, e);
            }
            catch { }

        }

        private void button11_Click_1(object sender, EventArgs e)
        {
            if ( timer1.Enabled )
            {
                button11.Text = "!Monitor!";
                timer1.Enabled = false;
                timer1.Stop();
            }else
            {
                //button6_Click(sender, e);
                button11.Text = "Monitor stop";
                timer1.Enabled = true;
                timer1.Start();
            }

        }
    }
}
