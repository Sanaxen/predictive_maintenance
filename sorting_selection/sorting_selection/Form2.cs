using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace sorting_selection_el
{
    public partial class Form2 : Form
    {
        public class CellPosition
        {
            public int Index { get; set; }
        }
        private void PictureBox_Click(object sender, EventArgs e)
        {
            var pictureBox = sender as PictureBox;
            if (pictureBox != null)
            {
                // Tagプロパティから行と列の情報を取得
                var position = pictureBox.Tag as CellPosition;
                if (position != null)
                {
                    form1.trackBar1.Value = position.Index;
                    form1.trackBar1_Scroll(sender, e);
                    //MessageBox.Show($"Clicked on Row: {position.Index}");
                }
            }
        }
        public Form2()
        {
            InitializeComponent();

        }
       public sorting_selection_el.Form1 form1 = null;

        public void View()
        {
            int n = listBox1.Items.Count;

            int N = 2;
            while (N * N < n) N++;

            if (N > 10) N = 10;
            // TableLayoutPanelの設定
            tableLayoutPanel1.ColumnCount = N;
            tableLayoutPanel1.RowCount = N;
            tableLayoutPanel1.ColumnStyles.Clear();
            tableLayoutPanel1.RowStyles.Clear();

            for (int i = 0; i < N; i++)
            {
                tableLayoutPanel1.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 1.0F / (float)tableLayoutPanel1.ColumnCount));
                tableLayoutPanel1.RowStyles.Add(new RowStyle(SizeType.Percent, 1.0F / (float)tableLayoutPanel1.RowCount));
            }

            int j = 0;
            // 画像を配置
            for (int row = 0; row < N; row++)
            {
                for (int col = 0; col < N; col++)
                {
                    string filePath = listBox1.Items[j].ToString();
                    var img = form1.CreateImage(filePath);

                    var pictureBox = new PictureBox
                    {
                        Image = img, // 画像のパスを指定
                        Dock = DockStyle.Fill,
                        SizeMode = PictureBoxSizeMode.Zoom
                    };

                    // TableLayoutPanelにPictureBoxを追加
                    tableLayoutPanel1.Controls.Add(pictureBox, col, row);
                    pictureBox.Tag = new CellPosition { Index = j };

                    // イベントハンドラの割り当て
                    pictureBox.Click += PictureBox_Click;
                    j++;
                    if (j == n) break;
                }
                if (j == n) break;
            }
            Show();
        }
        private void Form2_Load(object sender, EventArgs e)
        {

        }

        private void listBox1_SelectedIndexChanged(object sender, EventArgs e)
        {

        }
    }
}
