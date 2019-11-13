// xctyo1.js  --  cytoscript.js tutorial and experiments
//
// Goes with xcyto1.html.


var cy;

const layout = {
  name: 'cola',
  directed: true,
  fit: false,
  //handleDisconnected: false,
  //refresh: 0.00000001
}

function colaRun() {
  cy.elements().layout(layout).run();
}

function abb() {
  console.log('abb'); //DEBUG
  e = cy.nodes('#e');
  epos = e.position();
  epos.x += e.width() + 100;
  //cy.add({ position: epos, data: { id: 'new' }});
  cy.add({ data: { id: 'new' }});
  cy.add({ data: { id: 'e1', source: 'new', target: 'b' }});
  //cy.layout.stop();
  //cy.elements().layout({ name: 'cola' }).run();
  colaRun();
}

function ids() {
  for (i = 0; i < cy.nodes().length; i++) {
    id = cy.nodes()[i].id();
    posn = cy.nodes()[i].position();
    rposn = cy.nodes()[i].renderedPosition();
    console.log(i, ' ', id, ' ', posn, ' ', rposn);
  }
}


window.onload = function () {
  //document.getElementById('abb').addEventListener('click', abb);

  cy = cytoscape({
    container: document.getElementById('cy'),
    elements: [
      { data: { id: 'a' } },
      { data: { id: 'b', parent: 'e' } },
      { data: { id: 'c' } },
      { data: { id: 'd' } },
      { data: { id: 'e' } },
      { data: { id: 'f', parent: 'e' } },
      {
        data: {
          id: 'ab',
          source: 'a',
          target: 'b'
        }
      },
      {
        data: {
          id: 'cd',
          source: 'c',
          target: 'd'
        }
      },
      /*
      {
        data: {
          id: 'ef',
          source: 'e',
          target: 'f'
        }
      },
      */
      {
        data: {
          id: 'ac',
          source: 'a',
          target: 'c'
        }
      },
      /*
      {
        data: {
          id: 'be',
          source: 'b',
          target: 'e'
        }
      }
      */
      {
        data: {
          id: 'bf',
          source: 'b',
          target: 'f'
        }
      }
    ],
    layout: layout,
    randomize: false,
    style: [
      {
        selector: 'node',
        style: {
          shape: 'hexagon',
          'background-color': 'red',
          label: 'data(id)'
        }
      },
      {
        selector: 'node:parent',
        css: {
          'background-opacity': 0.333
        }
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle'
        }
      }
    ],
  });

  /*
  for (var i = 0; i < 10; i++) {
    cy.add({ data: { id: 'node' + i }});
    var source = 'node' + i;
    cy.add({
      data: {
        id: 'edge' + i,
        source: source,
        target: (i % 2 == 0 ? 'a' : 'b')
      }
    });
  }

  cy.layout({ name: 'cola' }).run();
  */

  ids(); //DEBUG
}
