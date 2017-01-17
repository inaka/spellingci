angular.module('spellingCI').controller('ReposController', ['$window', '$scope', 'Repo', 'Webhook', '$routeParams',
  function($window, $scope, Repo, Webhook, $routeParams){
    var controller = this;
    $scope.basecontroller.userLogged = $routeParams.user;
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

    controller.updateWebhook = function(repo) {
      repo.waiting = true;
      if(repo.status == "off") {
        var repoToSend = angular.copy(repo);
        repoToSend.status = "on";
        Webhook.add(repoToSend).success(function(data) {
          repo.status = "on";
          repo.waiting = false;
        });
      } else {
        var repoToSend = angular.copy(repo);
        repoToSend.status = "off";
        Webhook.remove(repoToSend).success(function(data) {
          repo.status = "off";
          repo.waiting = false;
        });
      }
    }

    function getRepos() {
      Repo.repos().success(function(data) {
        controller.repos = data;
        controller.wait = false;
      });
    }
  }]);
