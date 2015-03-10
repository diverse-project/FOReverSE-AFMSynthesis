/**
 * Created by gbecan on 3/2/15.
 */


afmApp.controller("AFMSynthesisController", function($scope, $http) {

});


afmApp.controller("Step1Controller", function($scope, $http) {

    $scope.variables= [];

    $scope.isFeature = function (index) {
        return $scope.variables[index].type == "Feature";
    }

});

