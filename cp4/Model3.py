

def run(problem):
    ws = problem_to_ws(problem)
    
    while True:
        ws = process_A(ws)
        ws = process_B(ws)
        if time_to_stop(ws):
            break
    print(ws.solution())


def process_A(ws) -> Workspace:
    new_ws_objs = ws.run_detectors()
    return ws.add_objs(new_ws_objs)

def process_B(ws) -> Workspace:
    p = choose_painter(ws)
    return p.run(ws)

# NEXT FakeIt to get ab_ working
# THEN incorporate other elements: unify, PainterCluster, PCMaker
