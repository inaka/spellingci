angular.module('spellingCI').controller('ReposController', ['$window', '$scope', 'Repo',
  function($window, $scope, Repo){
    var controller = this;
    controller.repos = [];
    getRepos();

    function getRepos() {
      Repo.repos().success(function(data) {
        controller.repos = data;
      });
    }
  }]);
