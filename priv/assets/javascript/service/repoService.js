angular.module('spellingCI').factory("Repo", ["$http", function TestFactory($http) {
  var basePath = "/repos";
    return {
      repos: function() {
        return $http({method: "GET", url: basePath});
      },
      sync: function() {
        return $http({method: "GET", url: basePath + "/sync"});
      }
    };
}]);
