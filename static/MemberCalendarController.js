var cbgApp = angular.module('cbgApp', []);

cbgApp.controller('MemberCalendarController', function($scope, $http) {
    $http.get('/mitglieder/kalender.json').success(function(data) {
        $scope.memberCalendarItems = data;
    });
    $scope.orderProp = 'zeit';
});
