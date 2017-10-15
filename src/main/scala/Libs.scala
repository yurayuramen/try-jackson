package libs
import com.fasterxml.jackson.databind.JsonSerializer
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.JsonDeserializer
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser
import scala.reflect.ClassTag


object Libs {
  
  import scala.reflect.runtime.{universe=>ru}
  
  class CaseObjectSerializer[T] extends JsonSerializer[T] {
    def serialize(caseObject:T,  jgen:JsonGenerator, provider:SerializerProvider)  {
      val className = caseObject.getClass.getSimpleName
      //case objectのクラス名をそのまま出すと、最後に「$」がついてしまう。
      //なんかいけてないので、最後の1文字を排除する
      jgen.writeString(className.slice(0, className.length() - 1))
      //jgen.write
    }
  }  
  
  class CaseObjectDeserializer[T: ru.TypeTag](baseClass:Class[T],  `type`:ru.Type , defaultValue:T = null
      , runtimeMirror:ru.RuntimeMirror = ru.runtimeMirror(this.getClass.getClassLoader))
      //
      //型パラメータの実行時の型消去を回避する
      // https://qiita.com/mather314/items/67fdbf8293edc0200444
      //
      ( implicit classTag: ClassTag[T] /*, typeTag:scala.reflect.runtime.universe.TypeTag[T] */)
  extends JsonDeserializer[T] 
  {
    
    
    //第二引数に渡された型（クラス）のサブモジュールを探す
    private[this] val mapKName_VModule=
    {
      //val parentTraitSymbol = typeTag.tpe.typeSymbol
      
      val parentTraitSymbol = ru.typeTag[T].tpe.typeSymbol //scala.reflect.runtime.universe.typeOf[T].typeSymbol
      
      `type`.decls.flatMap{symbol=> 
        
        if(symbol.isModule && symbol.info.baseClasses.contains(parentTraitSymbol)){
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
    }
    
    override def deserialize(jsonParser:JsonParser, cxt:DeserializationContext):T={
      val oc = jsonParser.getCodec()
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
  
  /***
   * jsonに数値型として埋め込みたいときに使用
   */
  trait HasMainAndSubCodeValue extends HasValue[(Int,Int)]{
    override val value:(Int,Int)
  }
  
  class CaseObjectSerializerWithValue[T <: HasValue[_]] extends JsonSerializer[T] {
    def serialize(caseObject:T,  jgen:JsonGenerator, provider:SerializerProvider)  {
      caseObject match{
        case hasStringValue:HasStringValue=>
          jgen.writeString(hasStringValue.value)
        case hasIntValue:HasIntValue=>
          jgen.writeNumber(hasIntValue.value)
        case hasMainAndSubCode:HasMainAndSubCodeValue=>
          //jgen.writeNumber(hasIntValue.value)
          val (mainCode,subCode)=hasMainAndSubCode.value
          jgen.writeStartObject()
          jgen.writeNumberField("mainCode", mainCode)
          jgen.writeNumberField("subCode", subCode)
          jgen.writeEndObject()
      }
    }
  }  

  
  class CaseObjectDeserializerWithValue[T2 ,T <: HasValue[T2] : ru.TypeTag](baseClass:Class[T], valueType:Class[T2],  `type`:scala.reflect.runtime.universe.Type , defaultValue:T = null
      , runtimeMirror:scala.reflect.runtime.universe.RuntimeMirror = scala.reflect.runtime.universe.runtimeMirror(this.getClass.getClassLoader)) 
      (implicit classTag1: scala.reflect.ClassTag[T], classTag2: scala.reflect.ClassTag[T2])
  extends JsonDeserializer[T] 
  {
    
    
    private[this] val mapK_VModule=
    {
      val parentTraitSymbol = ru.typeTag[T].tpe.typeSymbol
      
      `type`.decls.flatMap{symbol=> 
        if(symbol.isModule && symbol.info.baseClasses.contains(parentTraitSymbol)){
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
    }
    
    override def deserialize(jsonParser:JsonParser, cxt:DeserializationContext):T={
      val oc = jsonParser.getCodec()
      val rootNode:JsonNode = oc.readTree(jsonParser)
      
      
      val clazz = classTag2.runtimeClass 
      
      val key=
      if(clazz == classOf[String])
        rootNode.asText()
      else if(clazz == classOf[Int])
        rootNode.asInt()
      else if(clazz == classOf[(Int,Int)])
      {
        val mainCode=rootNode.at("/mainCode").asInt()
        val subCode=rootNode.at("/subCode").asInt()
        (mainCode,subCode)
      } 
      else
        throw new IllegalArgumentException("")
      
      mapK_VModule.get(key.asInstanceOf[T2]).getOrElse{
        defaultValue
      }
    }
  }
  
  
  
}