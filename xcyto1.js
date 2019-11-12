// xctyo1.js  --  cytoscript.js tutorial and experiments
//
// Goes with xcyto1.html.


var cy;

function abb() {
  console.log('abb'); //DEBUG
  e = cy.nodes('#e');
  epos = e.position();
  cy.add({ x: epos.x, y:epos.y, data: { id: 'new', x: epos.x, y: epos.y }});
  cy.add({ data: { id: 'e1', source: 'new', target: 'b' }});
  //cy.layout.stop();
  cy.elements().layout({ name: 'cola' }).run();
}

function ids() {
  for (i = 0; i < cy.nodes().length; i++) {
    console.log(cy.nodes()[i].id());
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
    layout: {
      name: 'cola',
      directed: true
    },
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
