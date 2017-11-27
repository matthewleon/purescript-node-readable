"use strict";

var stream = require("stream");

exports.newReadable = function (readCb) {
  return function() {
    var s = new stream.Readable({
      read: function(length) {
        readCb(s)(length)();
      }
    });
    return s;
  };
};

function pushChunk(s) {
  return function (chunk) {
    return function() {
      return s.push(chunk);
    };
  };
}

exports.pushBuffer = pushChunk;

exports.pushUint8Array = pushChunk;

exports.pushString = pushChunk;

exports.pushStringWithEncodingImpl = function (s) {
  return function (string) {
    return function (encoding) {
      return function() {
        return s.push(string, encoding);
      };
    };
  };
};

exports.pushEnd = pushChunk(null);
