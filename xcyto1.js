// xctyo1.js  --  cytoscript.js tutorial and experiments
//
// Goes with xcyto1.html.


var cy;

var alignments = {
  'a': {x: 0, y: 0},
  'c': {x: 0, y: -100},
  'd': {y: -100},
  'b': {y: 0},
  'f': {y: 0}
}

var gaps = [
  { axis: 'y', left: 'a', right: 'b', gap: 25 }
]

const layout = {
  name: 'cola',
  directed: true,
  fit: false,
  handleDisconnected: false,
  // alignment: function(n) { return alignments[n.id()]; },
  //gapInequalities: gaps,
  //refresh: 0.00000001
  refresh: 10
}

function colaRun() {
  cy.elements().layout(layout).run();
}

cIsDisplayed = true;

function toggleButton() {
  cIsDisplayed = !cIsDisplayed;
  cy.nodes('#c').style('display', cIsDisplayed ? 'element' : 'none');
  colaRun();
}

function abb() {
  console.log('abb'); //DEBUG
  e = cy.nodes('#e');
  epos = e.position();
  epos.x += e.width() + 100;
  //cy.add({ position: epos, data: { id: 'new' }});
  cy.add({ data: { id: 'new', label: 'This is the new node' }});
  cy.add({ data: { id: 'e1', source: 'new', target: 'b', weight: 12 }});
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
          shape: 'rectangle',
          //'background-color': 'red',
          'background-color': '#f0e0d0',
          label: 'data(id)',
          'text-valign': 'center',
          width: 'data(width)'
        }
      },
      {
        selector: 'node:parent',
        style: {
          'text-valign': 'top',
          'background-opacity': 0.333,
          'border-width': 0
        },
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'unbundled-bezier',
          'target-arrow-shape': 'triangle-backcurve',
          'line-color': 'green',
          'target-arrow-color': 'green',
          'width': "data(weight)"
        }
      }
    ],
  });

  cy.nodes().forEach(function(n) { n.data('width', 40); });
  cy.edges().forEach(function(e) { e.data('weight', 8); });
  /* To change the shape at run-time:
   *
   * c = cy.$('#c');
   * c.style('shape', 'ellipse');
   *
   * To change the width at run-time, the style must be set to read the width
   * from the node's data, e.g. width: 'data(width)'. Then you just change the
   * width element of the node's data at run-time.
   *
   * Similarly, to change a node's width at run-time:
   *
   * e = cy.$('ab');
   * e.data('weight', 8);
   */

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
