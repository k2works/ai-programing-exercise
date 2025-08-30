Feature: Application API

  Scenario: Application is running
    When I request GET "/api/rooms"
    Then the response status should be 200
