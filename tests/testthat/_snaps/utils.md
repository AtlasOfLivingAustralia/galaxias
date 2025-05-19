# cli_menu() basic usage

    Code
      cli_menu_with_mock(1)
    Message
      Found multiple thingies.
      Which one do you want to use?
      1: label a
      2: label b
      3: label c
      Selection: 1
    Output
      [1] 1

# cli_menu() does not infinite loop with invalid mocked input

    Code
      cli_menu_with_mock("nope")
    Message
      Found multiple thingies.
      Which one do you want to use?
      1: label a
      2: label b
      3: label c
      Selection: nope
      Enter a number between 1 and 3, or enter 0 to exit.
      Selection: 0
    Condition
      Error:
      ! Exiting...

# cli_menu() can work through multiple valid mocked inputs

    Code
      out <- cli_menu_with_mock(c(1, 3))
    Message
      Found multiple thingies.
      Which one do you want to use?
      1: label 1
      2: label 2
      3: label 3
      Selection: 1
      Found multiple thingies.
      Which one do you want to use?
      1: label 1
      2: label 2
      3: label 3
      Selection: 3

# cli_menu(), request exit via 0

    Code
      cli_menu_with_mock(0)
    Message
      Found multiple thingies.
      Which one do you want to use?
      1: label a
      2: label b
      3: label c
      Selection: 0
    Condition
      Error:
      ! Exiting...

# cli_menu(exit =) works

    Code
      cli_menu_with_mock(1)
    Message
      Hey we need to talk.
      What do you want to do?
      1: Give up
      2: Some other thing
      Selection: 1
    Condition
      Error:
      ! Exiting...

---

    Code
      cli_menu_with_mock(2)
    Message
      Hey we need to talk.
      What do you want to do?
      1: Give up
      2: Some other thing
      Selection: 2
    Output
      [1] 2

