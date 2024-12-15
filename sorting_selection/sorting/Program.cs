using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace sorting_el
{
    //"id","monotonicity","feature","lookback","lookback_slide","smooth_window","smooth_window_slide",
    ////"smooth_window2","smooth_window_slide2","sigin","max","min","image","filename_r"

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


    class Program
    {

        static void Main(string[] args)
        {
            string path = "";

            if ( args.Length == 0)
            {
                path = "..\\images";
            }else
            {
                path = args[0];
            }
            Console.WriteLine(path);

            bool s1 = File.Exists(path + "\\feature_summarys.csv");
            if ( !s1 )
            {
                return;
            }


            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

            List<Employee> csv = ReadCsvFile(path + "\\feature_summarys.csv");

            List<Employee> csv2 = new List<Employee>();
            int n = 0;

            {
                for (int i = 0; i < csv.Count; i++)
                {
                    string s = csv[i].filename_r;
                    Employee tmp = csv[i];
                    if (tmp.rmse12 >= 999999.0) break;
                    tmp.Confidence = 0;
                    tmp.Type = "green";
                    csv[i] = tmp;
                    csv2.Add(tmp);
                    n++;
                    if (n == 20) break;
                }
                Console.WriteLine(n);
                csv2.Sort((a, b) => Math.Sign(a.rmse12 - b.rmse12));
             }
            save_csv(path + "\\feature_summarys_best.csv", csv2);
        }




        static List<Employee> ReadCsvFile(string filePath)
        {
            List<Employee> employees = new List<Employee>();
            var lines = File.ReadAllLines(filePath, Encoding.GetEncoding("Shift_JIS"));

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
                employees.Add(employee);
            }

            return employees;
        }

        static void save_csv(string filePath,List<Employee> employees)
        {
            using (var writer = new StreamWriter(filePath, false, Encoding.GetEncoding("Shift_JIS")))
            {
                writer.WriteLine("id,monotonicity,feature," +
                   "lookback,lookback_slide,smooth_window,smooth_window_slide," +
                   "smooth_window2,smooth_window_slide2," +
                   "sigin,max,min,image,filename_r,rmse12,Confidence, Type");

                foreach (var employee in employees)
                {
                    writer.WriteLine($"{employee.id}, {employee.monotonicity},{employee.feature}," +
                        $"{employee.lookback},{employee.lookback_slide},{employee.smooth_window},{employee.smooth_window_slide}," +
                        $"{employee.smooth_window2},{employee.smooth_window_slide2}," +
                        $"{employee.sigin},{employee.max},{employee.min},{employee.image},{employee.filename_r},"+
                        $"{employee.rmse12},"+
                        $"{employee.Confidence},"+
                        $"{employee.Type}");
                }
            }
        }
    }
}
