// React
import { render } from 'react-dom';
import React from 'react';
import App from './components/App';
// Redux
import { Provider } from 'react-redux';
import store from './store';

// Render App with Provider
render(<Provider store={store}><App /></Provider>,
  document.getElementById('root'));
