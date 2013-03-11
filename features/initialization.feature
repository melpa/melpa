Feature: Initialization

  Scenario: read recipes
    Given read recipes

  Scenario: get archive alist
    Given set travis-archive-alist to archive alist
    Then travis-archive-alist should be empty

  Scenario: add to archive alist
    Given add "test1" to archive alist
    Then archive alist should be ("test1")
    Given add "test2" to archive alist
    Then archive alist should be ("test2" "test1")

  Scenario: remove from archive alist
    Given add "test1" to archive alist
    Given remove "test2" from archive alist
    Then archive alist should be ("test1")
    Given add "test2" to archive alist
    Then archive alist should be ("test2" "test1")
    Given remove "test1" from archive alist
    Then archive alist should be ("test2")
    

    
  