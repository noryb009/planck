package planck

import java.awt.Color

import scala.io.Source

object ObjReader {
  def read(file: String) = {
    def buildMesh(n: String, v: Seq[Vecd], f: Seq[Face]) = {
      if(v.nonEmpty)
        Some(Mesh(n, v, f))
      else
        None
    }

    val lines = Source.fromFile(file).getLines()

    val (meshes, vertices, faces, name) =
      lines.foldLeft(Seq[Mesh](), Seq[Vecd](), Seq[Face](), ""){case(r, s) =>
        val (m, v, f, n) = r

        val parts = s.split(" ", 2)
        if(parts.size < 2)
          r
        else parts(0) match {
          case "o" =>
            val newMesh = buildMesh(n, v, f)
            (newMesh.toSeq ++ m, Seq(), Seq(), parts(1))

          case "v" =>
            val indices = parts(1).split(" ")
            val vertex = Vecd.n(indices(0).toDouble, indices(1).toDouble, indices(2).toDouble)
            (m, vertex +: v, f, n)

          case "f" =>
            val indices = parts(1).split(" ")
            val face = Face(indices(0).toInt - 1, indices(1).toInt - 1, indices(2).toInt - 1)
            (m, v, face +: f, n)

          case _ => r
        }
    }

    buildMesh(name, vertices, faces).toSeq ++ meshes
  }
}
