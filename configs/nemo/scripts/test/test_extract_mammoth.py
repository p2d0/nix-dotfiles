import unittest
import sys
sys.path.append('../')
from extract_doc import get_out_dir

class extract_mammoth_test(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_(self):
        self.assertEquals("/home/andrew/Downloads/Где заказать мозаику ручной работы в Москве",
                          get_out_dir("/home/andrew/Downloads/Где заказать мозаику ручной работы в Москве.docx"))


if __name__ == '__main__':
    unittest.main()
