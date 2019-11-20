// fargcyto.js  --  Javascript code to display a FARG model
//
// Goes with fargcyto.html. Works with cytoscript.js and cola.js.

var cy;

// Layout object that gives constraints and other parameters to cola.js
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

// Runs the cola.js layout engine; call this after updating the graph.
function colaRun() {
  cy.elements().layout(layout).run();
}

// Button function: retrieves the FARG model from the server
function getModel() {
  $.get('getModel', updateGraphFromJSON);
}

function updateGraphFromJSON(data) {
  updateGraph(JSON.parse(data));
}

var gg;

function updateGraph(g) {
  gg = g;

  // Import nodes from server graph
  for (const sn of gg.nodes) {  // sn = node as represented on the server
    node = cy.$id(sn.id)        // node = cytoscape.js's representation of node
    if (node.empty()) {
      cy.add({group: 'nodes', data: {id: sn.id}});
    }
  }

  // Import edges from server graph
  for (const se of gg.links) {  // se = edge as represented on the server
    sid = makeEdgeId(
      se.source, se.source_port_label, se.target, se.target_port_label
    );
    edge = cy.$id(sid);
    if (edge.empty()) {
      cy.add({group: 'edges',
              data: { id: sid,
                      source: se.source,
                      source_port_label: se.source_port_label,
                      target: se.target,
                      target_port_label: se.target_port_label
                    }
              });
    }
  }
}

function makeEdgeId(source, source_port_label, target, target_port_label) {
  return (
    '' + source + '.' + source_port_label + '.' + target +
    '.' + target_port_label
  );
}

window.onload = function () {
  //document.getElementById('abb').addEventListener('click', abb);

  cy = cytoscape({
    container: document.getElementById('cy'),
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
}
