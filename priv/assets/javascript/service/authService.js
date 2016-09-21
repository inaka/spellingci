angular.module('spellingCI').factory("Auth", ["$http", function TestFactory($http) {
    return {
      logout: function() {
        return $http({method: "DELETE", url: "/logout"});
      }
    };
}]);
