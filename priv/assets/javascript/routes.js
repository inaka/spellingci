angular.module('spellingCI').config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/', {
        templateUrl: 'assets/templates/landing.html',
        controller: 'LandingController',
        controllerAs: 'landingCtrl'
      }).
      when('/repos/:user', {
        templateUrl: 'assets/templates/repos.html',
        controller: 'ReposController',
        controllerAs: 'repoCtrl'
      }).
      when('/about', {
        templateUrl: 'assets/templates/about.html'
      }).
      otherwise({
        redirectTo: '/'
    });
}]);
