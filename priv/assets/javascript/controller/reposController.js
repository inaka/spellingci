angular.module('spellingCI').controller('ReposController', ['$window', '$scope', 'Repo',
  function($window, $scope, Repo){
    var controller = this;
    controller.wait = true;
    controller.repos = [];
    getRepos();

    controller.sync = function() {
      controller.wait = true;
      Repo.sync().success(function(data) {
        controller.repos = data;
        controller.wait = false;
      });
    }

    function getRepos() {
      Repo.repos().success(function(data) {
        controller.repos = data;
        controller.wait = false;
      });
    }
  }]);
