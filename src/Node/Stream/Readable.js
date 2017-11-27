"use strict";

var stream = require("stream");

exports.newReadableImpl = function(options) {
  return function (readCb) {
    return function() {
      var s = new stream.Readable(Object.assign({}, options, {
        read: function(length) {
          readCb(s)(length)();
        }
      }));
      return s;
    };
  };
};

exports.pushImpl = function (rs) {
  return function (chunk) {
    return function() {
      return rs.push(chunk);
    };
  };
};

exports.pushStringWithEncodingImpl = function (rs) {
  return function (string) {
    return function (encoding) {
      return function() {
        return rs.push(string, encoding);
      };
    };
  };
};

exports.pushEndImpl = function(rs) {
  exports.pushImpl(rs)(null);
};
