Feature: Initialization

  Scenario: read recipes
    Given read recipes

  Scenario: get archive alist
    Given set travis-archive-alist to archive alist
    Then travis-archive-alist should be empty

