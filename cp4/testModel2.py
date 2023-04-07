## testModel2.py -- Unit tests for Model.py
#
#import unittest
#import inspect
#from pprint import pp
#if 'unittest.util' in __import__('sys').modules:
#    # Show full diff in self.assertEqual.
#    __import__('sys').modules['unittest.util']._MAX_LENGTH = 999999999
#
#class TestModel(unittest.TestCase):
#
#    def test_make_and_run_succ(self):
#        m = Model(initial_objects=set(Succ, ArgumentRelationDetector))
#        s1 = m.add_canvas('ab_')
#
#        m.run_detector(p1, (s1, 1), (s1, 2))
#        p2 = m.find_painter(Succ, dict(AA1=(s1, 1), AA2=(s1, 2)))
#        self.assertIsNotNone(p2)
#
#        m.run_detector(ArgumentRelationDetector, p2)
#        cluster_body = 
#        p3 = m.find_painter(PainterCluster, body=cluster_body)
#        self.assertIsNotNone(p3)
#
#        m.run_painter(p3)
#        s2 = m.get_competing_canvas_of(s1)
#        self.assertEqual(m.at(s2, Content), 'abc')
#
#    def test_make_and_run_succ(self):
#        m = Model()
#        c1 = m.add_canvas('ab_')
#
#        '''
#        See that 'ab' is a Succ relationship, make Succ(1, 2).
#        See that 1, 2 is a Succ relationship, make PainterCluster(
#            P1=Succ((S, I1), (S, I2))
#            Succ(I1, I2)
#        ),
#        See that the PainterCluster can make a painter to fill cell 3.
#        '''
#        
#        with m.catch_new():
#            m.run_detector(Succ, (c1, 1), (c1, 2))
#            p1 = m.get_new_object(Succ)
#            self.assertEqual(p1, WObj.mk(Succ, CC1=c1, II1=1, CC1=c1, II2=2))
#
#        with m.catch_new():
#            # Maybe later: Detectors that search only for specific kinds of
#            # argument relation.
#            m.run_detector(ArgumentRelationDetector, p1)
#            cluster_elems = set(
#                Define('PP1', WObj.mk(
#                    Succ, AA1='AA1', AA2='AA2'
#                )),
#                Define('AA1', WObj.mk(
#                    Address, SS='SS', II='II1'
#                )),
#                Define('AA2', WObj.mk(
#                    Address, SS='SS', II='II2'
#                )),
#                #WObj.mk(Succ, AA1='II1', AA2='II2')
#                Succ('II1', 'II2')
#            )
#            '''
#            cluster_elems = set(
#                Define('PP1', Succ.mk('AA1', 'AA2')),
#                Define('AA1', Address.mk('SS', 'II1')),
#                Define('AA2', Address.mk('SS', 'II2')),
#                Succ.mk('II1', 'II2')
#            )
#            PP1=Succ(AA1, AA2)
#            AA1=Succ(SS, II1)
#            AA2=Succ(SS, II2)
#            Succ(II1, II2)
#
#            PP1=Succ((SS, II1), (SS, II2))
#            Succ(II1, II2)
#
#            P1=Succ((S, I1), (S, I2))
#            Succ(I1, I2)
#            '''
#            p2 = m.get_new_object(PainterCluster)
#            self.assertEqual(m.at(p2, 'EL'), cluster_elems)
#
#        with m.catch_new():
#            m.run_painter(p2, SS=c1, II1=2, II3=3)
#            p3 = m.get_new_object(Succ)
#            expect = WObj.mk(Succ, CC1=c1, II1=2, CC2=c1, II2=3))
#            self.assertEqual(m.body(p3), expect)
#
#        with m.catch_new():
#            m.run_painter(p3)
#            c2 = m.get_new_object(Canvas)
#            expect = WObj.mk(Canvas, TT='abc', NN=3)
#            self.assertEqual(m.body(c2), expect)
#
#SuccNext(PP1, SS, II1, II2)
#PP1=Succ(SS, II1, SS, II2)
#SuccAddr(II1, II2)
#
#SuccNext(PP1, AA1, AA2, SS, II1, II2)=
#PP1=SuccAt(AA1, AA2)
#AA1=Address(SS, II1)
#AA2=Address(SS, II2)
#Succ(II1, II2)
