angular.module('spellingCI').factory("Webhook", ["$http", function TestFactory($http) {
    var baseUrl = "/webhook";
    return {
      add: function(repo) {
        return $http({method: "POST", url: baseUrl, data: repo});
      },
      remove: function(repo) {
        return $http({method: "DELETE", url: baseUrl, data: repo});
      }
    };
}]);
