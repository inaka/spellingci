angular.module('spellingCI').controller('ReposController', ['$window', '$scope', 'Repo', 'Webhook',
  function($window, $scope, Repo, Webhook){
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

    controller.updateWebhook = function(repo) {
      if(repo.checked) {
        var repoToSend = angular.copy(repo);
        repoToSend.status = "on";
        Webhook.add(repoToSend).success(function(data) {

        });
      } else {
        var repoToSend = angular.copy(repo);
        repoToSend.status = "off";
        Webhook.remove(repoToSend).success(function(data) {

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
