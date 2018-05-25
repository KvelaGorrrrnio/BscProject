import React, { Component } from 'react';
import '../styles/Dropdown.scss'

class Dropdown extends Component {

  renderItem(item,idx) {
    if (item.type == 'category') {
      return (<li key={idx}>{ item.title }
        <ul className='dropdown-list'>
          { item.children.map(this.renderItem.bind(this)) }
        </ul>
      </li>);
    } else {
      if (this.props.selected && (this.props.selected == idx || this.props.selected == item.title)) {
        return (
          <li key={idx} className='current'>{ item.title }</li>
        );
      } else {
        return (
          <li key={idx} onClick={item.onClick ? item.onClick : () => {}}>{ item.title }</li>
        );
      }
    }
  }

  render() {
    const disabled = this.props.disabled ? ' disabled' : '';
    const items = this.props.items ? this.props.items : [{ type: 'item', title: 'No items.' }];
    return (
      <div className={'dropdown' + disabled}>
        {this.props.children}
        <ul>
          { items.map(this.renderItem.bind(this)) }
        </ul>
      </div>
    );
  }
}

export default Dropdown;
