package macros

import testing.BaseSpec

final class PrintfMacroExampleSpec extends BaseSpec {
  import PrintfMacroExample._

  printf("Hi %d %s\n", 123, "abc")
  printf("Hello%f%d\n", 1.2, 3)

}
