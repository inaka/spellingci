angular.module('spellingCI').config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/', {
        templateUrl: 'assets/templates/landing.html'
      }).
      otherwise({
        redirectTo: '/'
    });
}]);
