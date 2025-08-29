Feature: Rooms API

  Background:
    Given I login with username "user1" and password "demo"

  Scenario: Get all rooms
    When I request GET /api/rooms
    Then the response status should be 200
    And the response should contain "roomId"
