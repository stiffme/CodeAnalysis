import com.ericsson.{HssFunctionProblem, HssCodeFileName, HssCodeFile}
import org.scalatest.FunSuite

/**
  * Created by esipeng on 2/25/2016.
  */
class HssCodeFileTest extends FunSuite{
  val resourcesDir = """C:\scala\TspCodeAnalysis\src\test\resources\"""


  test("Normal function should be parsed")  {
    val codeFile = new HssCodeFile(HssCodeFileName(resourcesDir + "good_example.cc","good_example.cc"))
    assert(codeFile.functions.size == 1)
    val f = codeFile.functions.head
    assert (f.enterLine == 12)
    assert (f.exitLine.size == 1)
    assert (f.exitLine.head == 15)
    assert (f.qualifiedName.equals("""Example::exmpleFunction1(int a, int b)"""))
    assert (f.problems.size == 0)
  }

  test("Comments should not influence code line analysis")  {
    val codeFile = new HssCodeFile(HssCodeFileName(resourcesDir + "good_example_comments.cc","good_example_comments.cc"))
    assert(codeFile.functions.size == 1)
    val f = codeFile.functions.head
    assert (f.enterLine == 12)
    assert (f.exitLine.size == 1)
    assert (f.exitLine.head == 15)
    assert (f.qualifiedName.equals("""Example::exmpleFunction1(int a, int b)"""))
    assert (f.problems.size == 0)
  }

  test("any instruction before ENTER should reported as a problem") {
    val codeFile = new HssCodeFile(HssCodeFileName(resourcesDir + "good_example_instruction_enter.cc","good_example_instruction_enter.cc"))
    assert(codeFile.functions.size == 1)
    val f = codeFile.functions.head
    assert (f.qualifiedName.equals("""Example::exmpleFunction1(int a, int b)"""))
    assert (f.problems.size == 1)
    assert (f.problems.head == HssFunctionProblem.FIRST_STATEMENT_NOT_ENTER)
  }


  test("no EXIT before return should reported as a problem") {
    val codeFile = new HssCodeFile(HssCodeFileName(resourcesDir + "good_example_instruction_exit.cc","good_example_instruction_exit.cc"))
    assert(codeFile.functions.size == 1)
    val f = codeFile.functions.head
    assert (f.qualifiedName.equals("""Example::exmpleFunction3(int a, int b)"""))
    assert (f.problems.size == 1)
    assert (f.problems.head == HssFunctionProblem.LAST_STATEMENT_NOT_EXIT)
  }

  test("single test") {
    val codeFile = new HssCodeFile(HssCodeFileName("""C:\HSSCode\hss16acp0lsv2\16acp0_lsv2\esm\HssEsm_SA\HssEsm_SE\HssEsmServices_BG\HssEsmS6aIncomingServices_OB\HssEsmS6aAppProcess_OU\src\HSS_EsmPurRequestHandler.cc""","HSS_EsmUpdateLastActivityTimeStampFSM.cc"))
    assert(codeFile.functions.size > 0)
  }
}
