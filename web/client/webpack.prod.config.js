const path = require('path');
const autoprefixer = require('autoprefixer');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const webpack = require('webpack');

module.exports = {
  devtool: 'cheap-module-source-map',
  entry: './src/index.jsx',
  output: {
    path: path.resolve(__dirname, 'build'),
    filename: 'bundle.js',
    chunkFilename: '[id].js',
    publicPath: '',
  },
  resolve: {
    extensions: ['.js', '.jsx'],
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        loader: 'babel-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.s?css$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            {
              loader: 'css-loader',
              options: {
                url: false,
                minimize: true,
                sourceMap: true,
              },
            },
            {
              loader: 'postcss-loader',
              options: {
                sourceMap: true,
                ident: 'postcss',
                plugins: () => [
                  autoprefixer({
                    browsers: [
                      '> 1%',
                      'last 2 versions',
                    ],
                  }),
                ],
              },
            },
            {
              loader: 'sass-loader',
              options: {
                sourceMap: true,
              },
            },
          ],
        })
      },
      {
        test: /\.(png|jpe?g|gif|svg)$/,
        loader: 'url-loader?limit=8000&name=images/[name].[ext]',
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(__dirname, '/src/index.html'),
      filename: 'index.html',
      inject: 'body',
    }),
    new ExtractTextPlugin('style.css'),
    new webpack.optimize.UglifyJsPlugin(),
    new webpack.DefinePlugin({
      'API_URL' : JSON.stringify('http://localhost:3001/api')
    }),
  ],
};
