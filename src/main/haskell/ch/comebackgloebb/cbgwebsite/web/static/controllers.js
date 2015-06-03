var santiApp = angular.module('santiApp', []);

santiApp.controller('RegistrationsController', function($scope, $http) {
    $http.get('reg.json').success(function(data) {
        $scope.registrations = data;
    });
    $scope.orderProp = 'zeit';
});
