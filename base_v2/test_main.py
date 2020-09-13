import numpy as np
import test_module as tm

np.random.seed(1)
print(np.random.rand())

for i in range(3):
    np.random.seed(4)
    print(i)
    print(np.random.rand())
    print(np.random.rand())
    print(np.random.rand())

np.random.seed(1)
print(np.random.rand())

np.random.seed(4)
tm.run_test()