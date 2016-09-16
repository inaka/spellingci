app = angular.module('spellingCI', ['ngRoute', 'ngResource', 'ui.bootstrap', 'ngCookies']);
app.config(['$httpProvider', function($httpProvider) {

    $httpProvider.interceptors.push(function($q) {
        return {
            'responseError': function(rejection){
                var defer = $q.defer();
                if(rejection.status == 401){
                    window.location.href = "/oauth/login";
                }
                defer.reject(rejection);
                return defer.promise;
            }
        };
    });

}]);
