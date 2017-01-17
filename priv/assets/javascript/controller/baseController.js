angular.module('spellingCI').controller('BaseController', ['$window', '$scope', '$cookies', 'Auth',
  function($window, $scope, $cookies, Auth){
    var controller = this;
    controller.logged = ($cookies.get("token") != undefined);
    $scope.basecontroller = {};
    $scope.basecontroller.userLogged = "";

    controller.logout = function() {
      Auth.logout().success(function(data) {
        $cookies.remove("token")
        window.location.href = "/";
      });
    };
  }]);
