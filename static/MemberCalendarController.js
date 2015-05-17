var cbgApp = angular.module('cbgApp', []);

cbgApp.controller('MemberCalendarController', function($scope, $http) {
    $http.get('/mitglieder/kalender').success(function(data) {
        $scope.memberCalendarItems = data;
    });
    $scope.orderProp = 'zeit';
});
