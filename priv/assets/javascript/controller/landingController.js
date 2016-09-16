angular.module('spellingCI').controller('LandingController', ['$window', '$scope', '$cookies',
  function($window, $scope, $cookies){
    var controller = this;

    if($cookies.get("token") != undefined) {
      window.location.href = "/#/repos";
    }

    controller.login = function() {
      window.location.href = "/oauth/login"
    };
  }]);
