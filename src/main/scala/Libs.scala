package libs
import com.fasterxml.jackson.databind.JsonSerializer
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.JsonDeserializer
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser


object Libs {
  
  class CaseObjectSerializer[T] extends JsonSerializer[T] {
    def serialize(caseObject:T,  jgen:JsonGenerator, provider:SerializerProvider)  {
      val className = caseObject.getClass.getSimpleName
      //case objectのクラス名をそのまま出すと、最後に「$」がついてしまう。
      //なんかいけてないので、最後の1文字を排除する
      jgen.writeString(className.slice(0, className.length() - 1))
      //jgen.write
    }
  }  
  
  
  class CaseObjectDeserializer[T](baseClass:Class[T],  `type`:scala.reflect.runtime.universe.Type , defaultValue:T = null
      , runtimeMirror:scala.reflect.runtime.universe.RuntimeMirror = scala.reflect.runtime.universe.runtimeMirror(this.getClass.getClassLoader))
      //
      //型パラメータの実行時の型消去を回避する
      // https://qiita.com/mather314/items/67fdbf8293edc0200444
      //
      (implicit classTag: scala.reflect.ClassTag[T])
  extends JsonDeserializer[T] 
  {
    
    private val mapKName_VModule=
    `type`.decls.flatMap{symbol=> 
      if(symbol.isModule){
        val module = symbol.asModule
        val any = runtimeMirror.reflectModule(module).instance
        any match{
          case instance:T=>
            Some ( module.name.toString() -> instance )
          case _=>
            None
        }
      }
      else
        None
    }.toMap
    
    override def deserialize(jsonParser:JsonParser, cxt:DeserializationContext):T={
      val oc = jsonParser.getCodec();
      val rootNode = oc.readTree(jsonParser):JsonNode
      val value = rootNode.asText()
      mapKName_VModule.get(value).getOrElse{
        //throw new NoSuchElementException(s"[${value}]に紐づくobjectがみつかりませんでした")
        defaultValue
      }
    }
    
    
  }
  
  
  /***
   * jsonに変換するとき、jsonからcase objectをデシリアライズするときの値を
   * カスタマイズできるようにした
   * 
   */
  sealed trait HasValue[T]{
    val value:T    
  }
  
  /***
   * jsonに文字列型として埋め込みたいときに使用
   */
  trait HasStringValue extends HasValue[String]{
    override val value:String
  }
  /***
   * jsonに数値型として埋め込みたいときに使用
   */
  trait HasIntValue extends HasValue[Int]{
    override val value:Int
  }
  
  class CaseObjectSerializerWithValue[T <: HasValue[_]] extends JsonSerializer[T] {
    def serialize(caseObject:T,  jgen:JsonGenerator, provider:SerializerProvider)  {
      caseObject match{
        case hasStringValue:HasStringValue=>
          jgen.writeString(hasStringValue.value)
        case hasIntValue:HasIntValue=>
          jgen.writeNumber(hasIntValue.value)
      }
    }
  }  
  
  class CaseObjectDeserializerWithStringValue[T <: HasStringValue](baseClass:Class[T],  `type`:scala.reflect.runtime.universe.Type , defaultValue:T = null
      , runtimeMirror:scala.reflect.runtime.universe.RuntimeMirror = scala.reflect.runtime.universe.runtimeMirror(this.getClass.getClassLoader)) 
      (implicit classTag: scala.reflect.ClassTag[T])
  extends JsonDeserializer[T] 
  {
    
    val mapKString_VModule=
    `type`.decls.flatMap{symbol=> 
      if(symbol.isModule){
        val module = symbol.asModule
        val any = runtimeMirror.reflectModule(module).instance
        any match{
          case instance:T=>
            Some ( instance.value -> instance )
          case _=>
            None
        }
      }
      else
        None
    }.toMap
    
    override def deserialize(jsonParser:JsonParser, cxt:DeserializationContext):T={
      val oc = jsonParser.getCodec();
      val rootNode:JsonNode = oc.readTree(jsonParser)
      val value = rootNode.asText()
      
      mapKString_VModule.get(value).getOrElse{
        defaultValue
      }
    }
  }
  
  class CaseObjectDeserializerWithIntValue[T <: HasIntValue](baseClass:Class[T],  `type`:scala.reflect.runtime.universe.Type , defaultValue:T = null
      , runtimeMirror:scala.reflect.runtime.universe.RuntimeMirror = scala.reflect.runtime.universe.runtimeMirror(this.getClass.getClassLoader)) 
      (implicit classTag: scala.reflect.ClassTag[T])
  extends JsonDeserializer[T] 
  {
    
    val mapKInt_VModule=
    `type`.decls.flatMap{symbol=> 
      if(symbol.isModule){
        val module = symbol.asModule
        val any = runtimeMirror.reflectModule(module).instance
        
        any match{
          case instance:T=>
            Some ( instance.value -> instance )
          case _=>
            None
        }
        
      }
      else
        None
    }.toMap
    
    override def deserialize(jsonParser:JsonParser, cxt:DeserializationContext):T={
      val oc = jsonParser.getCodec();
      val rootNode:JsonNode = oc.readTree(jsonParser)
      val value = rootNode.asInt()
      
      mapKInt_VModule.get(value).getOrElse{
        defaultValue
      }
    }
  }

  
  
}