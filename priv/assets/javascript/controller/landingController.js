angular.module('spellingCI').controller('LandingController', ['$window', '$scope',
  function($window, $scope){
    var controller = this;

    controller.login = function() {
      window.location.href = "/oauth/login"
    };
  }]);
