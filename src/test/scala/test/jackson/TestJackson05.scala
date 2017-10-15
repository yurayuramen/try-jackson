package test.jackson

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import libs.Libs._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.databind.PropertyNamingStrategy
import org.scalatest.FlatSpec


object Castles05 {
  
  sealed trait Castle extends HasMainAndSubCodeValue
  case object edo extends Castle
  {
    override val value = (1->1)
  }
  case object osaka extends Castle
  {
    override val value = (1->2)
  }
  case object azuchi extends Castle
  {
    override val value = (2->1)
  }
  case object other extends Castle
  {
    override val value = (99->99)
  }
}


object TestJackson05
{

  import Castles05._

  case class Parent01(name:String,militaryCommanders:Seq[MilitaryCommander01])

  case class MilitaryCommander01(name:Option[String],age:Option[Int]
  ,castle:Castle
  )
  
  case class Parent02(name:String,militaryCommanders:Seq[MilitaryCommander02])
  
  case class MilitaryCommander02(name:Option[String],age:Option[Int]
  ,castle:Option[Castle]
  )
  
  
}

class TestJackson05 extends FlatSpec with MustMatchers{
  
  import TestJackson05._
  import Castles05._
  
  val clazz = classOf[Castles05.Castle]
  val `type` = scala.reflect.runtime.universe.typeOf[Castles05.type]
  
  val customModule = new SimpleModule("CustomSerializer")
  .addSerializer(clazz, new CaseObjectSerializerWithValue)
  .addDeserializer(clazz, new CaseObjectDeserializerWithValue(clazz,classOf[(Int,Int)],`type`))
  //.addDeserializer(clazz, new CaseObjectDeserializerWithStringValue(clazz,`type`))
        
  val mapper=new ObjectMapper()
    .enable(SerializationFeature.INDENT_OUTPUT)
    .setSerializationInclusion(JsonInclude.Include.NON_NULL)
    //
    // https://github.com/fasterxml/jackson-databind/issues/811
    //
    .setSerializationInclusion(JsonInclude.Include.NON_ABSENT)
    //
    // https://stackoverflow.com/questions/5455014/ignoring-new-fields-on-json-objects-using-jackson
    //
    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    .registerModule(customModule)
    .registerModule(DefaultScalaModule)
  
  //it should "code5/Instance to json to Instance with HasStringValue" in{
      
    it should "Castleを非Option型で" in
    {
      val source = Parent01(name=s"リストその１",militaryCommanders=Seq(
        MilitaryCommander01(name=Some("秀吉"),age=Some(62),castle=osaka)    
        ,MilitaryCommander01(name=Some("家康"),age=Some(76),castle=edo)    
        ,MilitaryCommander01(name=Some("信長"),age=None,castle=azuchi)    
        ,MilitaryCommander01(name=Some("足軽その１"),age=None,castle=other)    
      )) 
      
      val json1 = mapper.writeValueAsString(source)
      println("json:${json1}")
      val result = mapper.readValue(json1, classOf[Parent01])
      println("instance:${result}")
      val json2 = mapper.writeValueAsString(result)
      println("json:${json2}")
      
      result.militaryCommanders(0).castle must ===(osaka)
      result.militaryCommanders(1).castle must ===(edo)
      result.militaryCommanders(2).castle must ===(azuchi)
      result.militaryCommanders(3).castle must ===(other)
      
      json1 must ===(json2)
      
    }
    
    it should " CastleをOption型で" in
    {
      val source = Parent02(name=s"リストその２",militaryCommanders=Seq(
        MilitaryCommander02(name=Some("秀吉"),age=Some(62),castle=Some(osaka))    
        ,MilitaryCommander02(name=Some("家康"),age=Some(76),castle=Some(edo))    
        ,MilitaryCommander02(name=Some("信長"),age=None,castle=Some(azuchi))    
        ,MilitaryCommander02(name=Some("足軽その１"),age=None,castle=None)    
      )) 
      
      val json1 = mapper.writeValueAsString(source)
      println(s"json:${json1}")
      val result = mapper.readValue(json1, classOf[Parent02])
      println(s"instance:${result}")
      val json2 = mapper.writeValueAsString(result)
      println(s"json:${json2}")
      
      result.militaryCommanders(0).castle.get must ===(osaka)
      result.militaryCommanders(1).castle.get must ===(edo)
      result.militaryCommanders(2).castle.get must ===(azuchi)
      result.militaryCommanders(3).castle must ===(None)
      
      json1 must ===(json2)
      
    }
    
  
  
  
  
}