angular.module('spellingCI').factory("Auth", ["$http", function TestFactory($http) {
    return {
      logout: function(question) {
        return $http({method: "DELETE", url: "/logout"});
      }
    };
}]);
