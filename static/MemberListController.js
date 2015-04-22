var cbgApp = angular.module('cbgApp', []);

cbgApp.controller('MemberListController', function($scope, $http) {
    $http.get('/mitglieder/liste.json').success(function(data) {
        $scope.memberListItems = data;
    });
    $scope.orderProp = 'name';
});
