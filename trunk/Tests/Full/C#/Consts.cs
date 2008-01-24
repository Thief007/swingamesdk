using System;
using System.Collections.Generic;
using System.Text;

namespace Tests
{
    class Consts
    {
		public const int TITLE_TEXT_TOP = 10;
		public const int TITLE_HEIGHT = 50;
		public const int SCREEN_WIDTH = 800;
		public const int SCREEN_HEIGHT = 600;
		public const int METHOD_TOP = TITLE_HEIGHT + 5;
		public const int METHOD_LEFT = 5;
		public const int METHOD_HEIGHT = 20;
		public const int INSTRUCTION_WIDTH = 300;
		public const int GENERAL_INST_HEIGHT = 20;

		public const int INSTRUCTION_LEFT = SCREEN_WIDTH - INSTRUCTION_WIDTH;
		public const int INSTRUCTION_TOP = TITLE_HEIGHT + METHOD_HEIGHT + 50;
		public const int INSTRUCTION_HEIGHT = SCREEN_HEIGHT - INSTRUCTION_TOP - GENERAL_INST_HEIGHT;
		public const int GENERAL_INST_LEFT = 5;
		public const int GENERAL_INST_TOP = SCREEN_HEIGHT - GENERAL_INST_HEIGHT;
		public const int GENERAL_INST_WIDTH = SCREEN_WIDTH;
		public const int TEST_IN_LEFT = 0;
		public const int TEST_IN_TOP = TITLE_HEIGHT + METHOD_HEIGHT;
		public const int TEST_IN_WIDTH = SCREEN_WIDTH - INSTRUCTION_WIDTH;
        public const int TEST_IN_HEIGHT = SCREEN_HEIGHT - TEST_IN_TOP - GENERAL_INST_HEIGHT;
    }
}
