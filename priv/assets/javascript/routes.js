angular.module('spellingCI').config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/', {
        templateUrl: 'assets/templates/landing.html',
        controller: 'LandingController',
        controllerAs: 'landingCtrl'
      }).
      when('/repos', {
        templateUrl: 'assets/templates/repos.html',
        controller: 'ReposController',
        controllerAs: 'repoCtrl'
      }).
      otherwise({
        redirectTo: '/'
    });
}]);
