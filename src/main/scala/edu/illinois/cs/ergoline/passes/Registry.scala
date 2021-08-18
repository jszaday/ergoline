package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.util.TopologicalSort

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

object Registry {
  private val members: mutable.HashMap[ClassTag[_], Pass] = new mutable.HashMap

  def registered: List[Pass] = members.values.toList

  def onLoad: List[Pass] =
    TopologicalSort.sort(registered.filter(_.phase == Phase.Load))

  def onOptimize: List[Pass] =
    TopologicalSort.sort(registered.filter(_.phase == Phase.Optimize))

  def instance[A <: Pass: ClassTag]: A = {
    val tag = classTag[A]
    if (!members.contains(tag)) {
      val const = tag.runtimeClass.getDeclaredConstructor()
      val inst = const.newInstance().asInstanceOf[A]
      members.put(tag, inst)
      inst
    } else {
      members(tag).asInstanceOf[A]
    }
  }
}
