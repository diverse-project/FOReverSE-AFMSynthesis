/**
 * Created by gbecan on 3/2/15.
 */


var afmApp = angular.module("AFMSynthesisApp", ['ngRoute']);

// configure our routes
afmApp.config(function($routeProvider) {
    $routeProvider
        .when('/', {
            templateUrl : 'home',
            controller  : 'HomeController'
        })
        .when('/load', {
            templateUrl : 'load',
            controller  : 'LoadController'
        })
        .when('/variables', {
            templateUrl : 'variables',
            controller  : 'PlopController'
        })
        .when('/hierarchy', {
            templateUrl : 'hierarchy',
            controller  : 'PlopController'
        })
        .when('/groups', {
            templateUrl : 'groups',
            controller  : 'PlopController'
        })
        .when('/constraints', {
            templateUrl : 'constraints',
            controller  : 'PlopController'
        })
        .when('/afm', {
            templateUrl : 'afm',
            controller  : 'PlopController'
        })

});

afmApp.controller("AFMSynthesisController", function($scope, $http) {

});


afmApp.controller("HomeController", function($scope, $http) {
    console.log("home");
});


afmApp.controller("LoadController", function($scope, $http) {
    console.log("load");
});

afmApp.controller("PlopController", function($scope, $http) {
    console.log("plop");
});

var synthesizerSocket = new WebSocket("ws://" + location.host + "/synthesize");

synthesizerSocket.onopen = function() {
    console.log("open websocket");
    synthesizerSocket.send(JSON.stringify(["f1", "f2"]));
};

synthesizerSocket.onclose = function() {
    console.log("close websocket")
};

synthesizerSocket.onmessage = function(msg) {
    console.log("message: " + msg.data)
};

