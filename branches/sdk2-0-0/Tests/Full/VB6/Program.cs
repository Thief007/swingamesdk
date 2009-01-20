using System;
using System.Collections.Generic;

using SwinGameVB;
using System.Reflection;

namespace Tests
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main()
        {
            //Opens the Consts.Graphics Window
           Consts.Core.OpenGraphicsWindow("SwinGameSDK Showcase", 800, 600);

            //Opens the Audio System
           Consts.Audio.OpenAudio();

            //Plays the SwinGameSDK Intro
            GameResources.LoadResources();
           
            //Run Tests
            List<TestSuite> suites = new List<TestSuite>();
            LoadTestSuites(suites);

            System.IO.TextWriter writer;

            using (writer = new System.IO.StreamWriter("results.log"))
            {
                foreach (TestSuite ts in suites)
                {
                    ts.Run();
                    ts.SaveResults(writer);
                }
            }

            //Free the resoources
            GameResources.FreeResources();

            //Closes the Audio System
           Consts.Audio.CloseAudio();
        }

        private static void LoadTestSuites(List<TestSuite> suites)
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
            Type[] types = assembly.GetTypes();

            foreach (Type t in types)
            {
                if( t.IsClass && t.GetInterface("IGameTestLoader") != null)
                {
                    IGameTestLoader gl;
                    gl = Activator.CreateInstance(t) as IGameTestLoader;
                    gl.AddTo(suites);
                }
            }
        }
    }
}