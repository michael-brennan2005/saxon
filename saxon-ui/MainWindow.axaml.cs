using System;
using Avalonia.Controls;
using saxon;

namespace saxon_ui
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            var wrapper = new Wrapper.SaxonWrapper();
            while (true)
            {
                Console.Out.WriteLine("Input:");
                var input = Console.In.ReadLine();
                var val = wrapper.runInput(input);
                Console.Out.WriteLine($"{val}");
            }
        }
    }
}