Feature: Toggle Single Line Conditional

  Background:
    Given the buffer is empty

  Scenario: Toggle Multi Line to Single Line
    When I insert:
      """
      if foo():
        bar()
      """
    And I press "C-p"
    When I call "boo-toggle-single-line-control-flow"
    Then I should see:
      """
      bar() if foo()
      """
    And I press "C-x C-s cry"


  Scenario: Toggle Multi Line to Single Line in function
    When I insert:
      """
      def Foo():
        if bar():
          return baz()
      """
    And I press "C-p"
    When I call "boo-toggle-single-line-control-flow"
    Then I should see:
      """
      def Foo():
        return baz() if foo()
      """
    And I press "C-x C-s weep"


