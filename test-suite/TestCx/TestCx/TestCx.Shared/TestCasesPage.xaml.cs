using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;
using Cxns;
// The Blank Page item template is documented at http://go.microsoft.com/fwlink/?LinkId=234238

namespace TestCx
{
    /// <summary>
    /// An empty page that can be used on its own or navigated to within a Frame.
    /// </summary>
    class Client : ClientInterface
    {
        public ClientReturnedRecord GetRecord(long RecordId, string Utf8string, string Misc)
        {
            ClientReturnedRecord r = new ClientReturnedRecord
            {
                RecordId = RecordId,
                Content = Utf8string,
                Misc = Misc
            };
            return r;
        }

        public double IdentifierCheck(byte[] Data, int R, long Jret)
        {
            throw new NotImplementedException();
        }

        public string ReturnStr()
        {
            throw new NotImplementedException();
        }
    }

    public sealed partial class TestCasesPage : Page
    {
        private string testResult;
        public TestCasesPage()
        {
            this.InitializeComponent();
            testRecord();
            testPrimitiveList();
            testNestedCollection();
            testMap();
            testEmptyMap();
            testMapListRecord();
            testClientInterfaceAnsi();
            testClientInterfaceNonansi();
            testEnumMap();
            testToken();
            testReturnNone();
            testAssortedPrimitives();
            testBinary();
            output.Text = testResult;
        }

        private void testBinary()
        {
            byte[] binary = new byte[] { 1, 2, 3};
            byte[] binary2 = TestHelpers.IdBinary(binary);
            append("testBinary", binary[0] == binary2[0] && binary[1] == binary2[1] && binary[2] == binary2[2]);
        }

        private void testAssortedPrimitives()
        {
            var p = TestHelpers.AssortedPrimitivesId(new AssortedPrimitives(true, 8, 16, 32, 64, 32.0f, 64.0, false, null, 16, null, 64, null, 64.0));
            append("testAssortedPrimitives",
                p.B == true &&
                p.Eight == 8 &&
                p.Sixteen == 16 &&
                p.Thirtytwo == 32 &&
                p.Sixtyfour == 64 &&
                p.Fthirtytwo == 32.0f &&
                p.Fsixtyfour == 64.0 &&
                p.OB.Value == false &&
                p.OEight.HasValue == false &&
                p.OSixteen.Value == 16 &&
                p.OThirtytwo.HasValue == false &&
                p.OSixtyfour.Value == 64 &&
                p.OFthirtytwo.HasValue == false &&
                p.OFsixtyfour.Value == 64.0);
        }

        private void testReturnNone()
        {
            append("testReturnNone", false == TestHelpers.ReturnNone().HasValue);
        }

        private void testToken()
        {
            var cppToken = TestHelpers.CreateCppToken();
            append("testTokenId", TestHelpers.TokenId(cppToken).Whoami());
            append("testCppToken", TestHelpers.CppTokenId(cppToken), cppToken.Whoami());
            try
            {
                TestHelpers.CheckCppToken(cppToken);
                TestHelpers.CheckTokenType(cppToken, cppToken.Whoami());
                append("testCppToken & testTokenType", true);
            }
            catch(Exception e)
            {
                append("testCppToken & testTokenType", false, e.Message);
            }
        }

        private void testEnumMap()
        {
            Dictionary<Color, string> m = new Dictionary<Color, string>
            {
                {Color.Red, "red" },
                {Color.Orange, "orange" },
                {Color.Yellow, "yellow" },
                {Color.Green, "green" },
                {Color.Blue, "blue" },
                {Color.Indigo, "indigo" },
                {Color.Violet, "violet" }
            };
            try
            {
                TestHelpers.CheckEnumMap(m);
                append("testEnumMap", true);
            }
            catch(Exception e)
            {
                append("testEnumMap", false, e.Message);
            }
        }

        private void testClientInterfaceNonansi()
        {
            try
            {
                TestHelpers.CheckClientInterfaceNonascii(new Client());
            }
            catch(Exception e)
            {
                append("testClientInterfaceNonansi", false, e.Message);
            }
        }

        private void testClientInterfaceAnsi()
        {
            try
            {
                TestHelpers.CheckClientInterfaceAscii(new Client());
                append("testClientInterfaceAnsi", true);
            }
            catch (Exception e)
            {
                append("testClientInterfaceNonansi", e.Message);
            }
        }

        private void testMapListRecord()
        {
            var p = TestHelpers.GetMapListRecord();
            var r = TestHelpers.CheckMapListRecord(p);
            append("testMapListRecord", r);
        }
        private void testEmptyMap()
        {
            var p = TestHelpers.GetEmptyMap();
            var r = TestHelpers.CheckEmptyMap(p);
            append("testEmptyMap", r);
        }

        private void testMap()
        {
            var p = TestHelpers.GetMap();
            var r = TestHelpers.CheckMap(p);
            append("testMap", r);
        }
        private void testNestedCollection()
        {
            var p = TestHelpers.GetNestedCollection();
            var r = TestHelpers.CheckNestedCollection(p);
            append("testNestedCollection", r);
        }

        private void testPrimitiveList()
        {
            var p = TestHelpers.GetPrimitiveList();
            bool r = TestHelpers.CheckPrimitiveList(p);
            append("testPrimitiveList", r);
        }

        private void testRecord()
        {
            var rec = TestHelpers.GetSetRecord();
            bool r = TestHelpers.CheckSetRecord(rec);
            append("testRecord", r);
        }
        private void append(params object[] p)
        {
            foreach(var i in p)
            {
                testResult += i.ToString() + " ";
            }
            testResult += "\r\n";
        }
    }
}
