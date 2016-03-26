# Planck

Planck is a 3D software renderer written in Scala.
There is a heavy emphasis on using immutable types, higher order functions,
and not using external libraries.
The third point means that vector, matrix and quaternion classes needed to be
developed, as well as a simple .obj file reader.

The engine uses the right hand rule. By default, X goes right, Y goes up and Z
goes towards you.

In the future, I hope to:
* Support textures.
* Support lights.
* Improve performance.

The code is loosely based around
[this tutorial](https://blogs.msdn.microsoft.com/davrous/2013/06/13/tutorial-series-learning-how-to-write-a-3d-soft-engine-from-scratch-in-c-typescript-or-javascript/).
