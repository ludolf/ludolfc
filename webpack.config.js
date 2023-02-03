const path = require('path');

module.exports = {
  entry: './src/ludolfc.js',
  output: {
    filename: 'ludolfc.js',
    path: path.resolve(__dirname, 'dist'),
    globalObject: 'this',
    library: {
      name: 'ludolfc',
      type: 'umd',
    },
  },
};