angular.module('spellingCI').factory("Repo", ["$http", function TestFactory($http) {
  var basePath = "/repos";
    return {
      repos: function(question) {
        return $http({method: "GET", url: basePath});
      }
    };
}]);
